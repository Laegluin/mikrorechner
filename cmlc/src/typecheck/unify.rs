use crate::support;
use crate::typecheck::{Field, Function, Record, Type, TypeDesc, TypeError, TypeRef};
use std::mem;
use std::rc::Rc;

/// The environment holding all types during type checking. The types are stored as disjoint-set
/// (see <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>) backed by a Vec. Because
/// of this unused nodes are only freed when the whole set is dropped. This is fine, since there
/// won't be enough nodes for this to matter; in return, everything is stored in a nice, flat piece of memory.
///
/// Merging of two types is done using a modified type inference algorithm for a Hindley-Milner type system.
/// (see <https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Degrees_of_freedom_instantiating_the_rules>).
///
/// Notably, there are special types that coerce to others like `Never` or `Int`.
#[derive(Debug)]
pub struct TypeEnv {
    nodes: Vec<Node>,
}

#[derive(Debug)]
enum Node {
    Root(Root),
    Next(usize),
}

#[derive(Debug)]
struct Root {
    var: Type,
    ty_ref: TypeRef,
    rank: u8,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv { nodes: Vec::new() }
    }

    pub fn insert(&mut self, var: Type) -> TypeRef {
        let ty_ref = TypeRef::new(self.nodes.len(), &var);

        self.nodes.push(Node::Root(Root {
            var,
            ty_ref: ty_ref.clone(),
            rank: 0,
        }));

        ty_ref
    }

    pub fn find_type(&self, ty: &TypeRef) -> (TypeRef, &Type) {
        let root = self.find(ty.0);
        (root.ty_ref.clone(), &root.var)
    }

    fn find(&self, node: usize) -> &Root {
        assert!(node != TypeRef::invalid().0);

        match self.nodes[node] {
            Node::Next(next_node) => self.find(next_node),
            Node::Root(ref root) => root,
        }
    }

    fn find_mut(&mut self, node: usize) -> (usize, &mut Root) {
        assert!(node != TypeRef::invalid().0);

        let start_node = node;
        let mut node = node;

        // using the ugly iterative version is required to make this compile while also using path compression,
        // because compressing the path requires a non overlapping write to the vec
        loop {
            match self.nodes[node] {
                Node::Next(next_node) => node = next_node,
                Node::Root(_) => {
                    // use path compression
                    if start_node != node {
                        self.nodes[start_node] = Node::Next(node);
                    }

                    match self.nodes[node] {
                        Node::Root(ref mut root) => return (node, root),
                        // we already confirmed the variant above, and neither
                        // `node` nor the actual value in the vec have changed
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    pub fn unify(&mut self, expected: &TypeRef, actual: &TypeRef) -> Result<TypeRef, TypeError> {
        let (expected_idx, expected_root) = self.find_mut(expected.0);
        let expected_rank = expected_root.rank;
        let expected_var = mem::replace(&mut expected_root.var, Type::Var);

        let (actual_idx, actual_root) = self.find_mut(actual.0);
        let actual_rank = actual_root.rank;
        let actual_var = mem::replace(&mut actual_root.var, Type::Var);

        // if the two refs refer to the same type, just return one of them
        // make sure to replace the type variable with the original again
        if expected_idx == actual_idx {
            mem::replace(&mut actual_root.var, expected_var);
            return Ok(expected.clone());
        }

        let merged = self.merge(expected_var, actual_var)?;

        if expected_rank < actual_rank {
            let ty_ref = TypeRef::new(actual_idx, &merged);

            self.nodes[expected_idx] = Node::Next(actual_idx);
            self.nodes[actual_idx] = Node::Root(Root {
                var: merged,
                ty_ref: ty_ref.clone(),
                rank: actual_rank,
            });

            Ok(ty_ref)
        } else {
            let ty_ref = TypeRef::new(expected_idx, &merged);

            self.nodes[actual_idx] = Node::Next(expected_idx);
            self.nodes[expected_idx] = Node::Root(Root {
                var: merged,
                ty_ref: ty_ref.clone(),
                rank: if expected_rank == actual_rank {
                    expected_rank + 1
                } else {
                    expected_rank
                },
            });

            Ok(ty_ref)
        }
    }

    fn merge(&mut self, expected: Type, actual: Type) -> Result<Type, TypeError> {
        match (expected, actual) {
            // always choose the other one, it can never be less specific
            (Type::Var, other) | (other, Type::Var) => Ok(other),
            // Never is the subtype of all other types (except Any), so use the more specific subtype
            (Type::Never, other) | (other, Type::Never) => Ok(other),
            // choose the more specific int
            (Type::Int, int @ Type::U32)
            | (Type::Int, int @ Type::I32)
            | (int @ Type::U32, Type::Int)
            | (int @ Type::I32, Type::Int) => Ok(int),
            // all non-generic types can always be merged as themselves
            (eq @ Type::Int, Type::Int)
            | (eq @ Type::Bool, Type::Bool)
            | (eq @ Type::I32, Type::I32)
            | (eq @ Type::U32, Type::U32)
            | (eq @ Type::Str, Type::Str) => Ok(eq),
            // pointer coerces to the more specific pointer type (const, mut or it stays a generic pointer)
            (Type::Ptr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.unify(&expected_inner, &actual_inner)?;
                Ok(Type::Ptr(inner))
            }
            (Type::Ptr(expected_inner), Type::MutPtr(actual_inner))
            | (Type::MutPtr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.unify(&expected_inner, &actual_inner)?;
                Ok(Type::MutPtr(inner))
            }
            (Type::Ptr(expected_inner), Type::ConstPtr(actual_inner))
            | (Type::ConstPtr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.unify(&expected_inner, &actual_inner)?;
                Ok(Type::ConstPtr(inner))
            }
            // mut pointers can be coerced to const pointers
            (Type::ConstPtr(expected_inner), Type::ConstPtr(actual_inner))
            | (Type::ConstPtr(expected_inner), Type::MutPtr(actual_inner)) => {
                let inner = self.unify(&expected_inner, &actual_inner)?;
                Ok(Type::ConstPtr(inner))
            }
            // mut pointers are only equal to themselves (given equal pointees)
            (Type::MutPtr(expected_inner), Type::MutPtr(actual_inner)) => {
                let inner = self.unify(&expected_inner, &actual_inner)?;
                Ok(Type::MutPtr(inner))
            }
            // for generic types, unify the type parameters first
            (Type::Array(expected_inner, expected_len), Type::Array(actual_inner, actual_len)) => {
                if expected_len != actual_len {
                    return Err(TypeError::Mismatch(
                        Rc::new(TypeDesc::from_type(&Type::Array(
                            expected_inner,
                            expected_len,
                        ))),
                        Rc::new(TypeDesc::from_type(&Type::Array(actual_inner, actual_len))),
                    ));
                }

                let inner = self.unify(&expected_inner, &actual_inner)?;
                Ok(Type::Array(inner, expected_len))
            }
            (Type::Tuple(expected_inner), Type::Tuple(actual_inner)) => {
                if expected_inner.len() != actual_inner.len() {
                    return Err(TypeError::Mismatch(
                        Rc::new(TypeDesc::from_type(&Type::Tuple(expected_inner))),
                        Rc::new(TypeDesc::from_type(&Type::Tuple(actual_inner))),
                    ));
                }

                let inner = expected_inner
                    .into_iter()
                    .zip(actual_inner)
                    .map(|(expected, actual)| self.unify(&expected, &actual))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Type::Tuple(inner))
            }
            // function are like tuples plus an additional return type
            (Type::Function(expected_fn), Type::Function(actual_fn)) => {
                if expected_fn.params.len() != actual_fn.params.len() {
                    return Err(TypeError::Mismatch(
                        Rc::new(TypeDesc::from_type(&Type::Function(expected_fn))),
                        Rc::new(TypeDesc::from_type(&Type::Function(actual_fn))),
                    ));
                }

                let params = expected_fn
                    .params
                    .into_iter()
                    .zip(actual_fn.params)
                    .map(|(expected, actual)| self.unify(&expected, &actual))
                    .collect::<Result<Vec<_>, _>>()?;

                let ret = self.unify(&expected_fn.ret, &actual_fn.ret)?;

                Ok(Type::Function(Function { params, ret }))
            }
            (Type::PartialRecord(expected_fields), Type::PartialRecord(mut actual_fields)) => {
                let mut unified_fields = Vec::new();

                // go over all expected fields, if they are also in the actual fields, unify their
                // types, otherwise just add them
                for expected_field in expected_fields {
                    let field_match = support::find_remove(&mut actual_fields, |field| {
                        expected_field.name == field.name
                    });

                    match field_match {
                        Some(actual_field) => {
                            unified_fields.push(Field {
                                name: expected_field.name,
                                ty: self.unify(&expected_field.ty, &actual_field.ty)?,
                            });
                        }
                        None => {
                            unified_fields.push(expected_field);
                        }
                    }
                }

                // if there are remaining fields in actual, just add them since the do not
                // intersect with expected
                for actual_field in actual_fields {
                    unified_fields.push(actual_field);
                }

                Ok(Type::PartialRecord(unified_fields))
            }
            (Type::Record(record), Type::PartialRecord(mut fields))
            | (Type::PartialRecord(mut fields), Type::Record(record)) => {
                let mut unified_fields = Vec::new();

                // unify types if two fields overlap
                for record_field in record.fields {
                    let field_match =
                        support::find_remove(&mut fields, |field| record_field.name == field.name);

                    match field_match {
                        Some(field) => {
                            unified_fields.push(Field {
                                name: record_field.name,
                                ty: self.unify(&record_field.ty, &field.ty)?,
                            });
                        }
                        None => {
                            unified_fields.push(record_field);
                        }
                    }
                }

                if let Some(field) = fields.pop() {
                    return Err(TypeError::UnknownFieldName(field.name));
                }

                Ok(Type::Record(Record {
                    id: record.id,
                    name: record.name,
                    fields: unified_fields,
                }))
            }
            // nominal types are always fully known, so they can be compared directly
            (Type::Record(expected_record), Type::Record(actual_record)) => {
                if expected_record == actual_record {
                    Ok(Type::Record(expected_record))
                } else {
                    Err(TypeError::Mismatch(
                        Rc::new(TypeDesc::from_type(&Type::Record(expected_record))),
                        Rc::new(TypeDesc::from_type(&Type::Record(actual_record))),
                    ))
                }
            }
            // catch all for all types that definitely cannot be unified
            (expected, actual) => Err(TypeError::Mismatch(
                Rc::new(TypeDesc::from_type(&expected)),
                Rc::new(TypeDesc::from_type(&actual)),
            )),
        }
    }
}
