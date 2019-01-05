use crate::support;
use crate::typecheck::{Function, Param, Type, TypeError, TypeRef};
use std::mem;

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
    rank: u8,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv { nodes: Vec::new() }
    }

    pub fn insert(&mut self, var: Type) -> TypeRef {
        self.nodes.push(Node::Root(Root { var, rank: 0 }));
        TypeRef(self.nodes.len() - 1)
    }

    pub fn new_type_var(&mut self) -> TypeRef {
        self.insert(Type::Var)
    }

    fn find(&mut self, node: usize) -> (usize, &mut Root) {
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

    pub fn unify(&mut self, expected: TypeRef, actual: TypeRef) -> Result<TypeRef, TypeError> {
        let (expected_idx, expected_root) = self.find(expected.0);
        let expected_rank = expected_root.rank;
        let expected_var = mem::replace(&mut expected_root.var, Type::Var);

        let (actual_idx, actual_root) = self.find(actual.0);
        let actual_rank = actual_root.rank;
        let actual_var = mem::replace(&mut actual_root.var, Type::Var);

        if expected_idx == actual_idx {
            return Ok(TypeRef(expected_idx));
        }

        let merged = self.merge(expected_var, actual_var)?;

        if expected_rank < actual_rank {
            self.nodes[expected_idx] = Node::Next(actual_idx);
            self.nodes[actual_idx] = Node::Root(Root {
                var: merged,
                rank: actual_rank,
            });

            Ok(TypeRef(actual_idx))
        } else {
            self.nodes[actual_idx] = Node::Next(expected_idx);
            self.nodes[expected_idx] = Node::Root(Root {
                var: merged,
                rank: if expected_rank == actual_rank {
                    expected_rank + 1
                } else {
                    expected_rank
                },
            });

            Ok(TypeRef(expected_idx))
        }
    }

    // TODO: impl error handling
    fn merge(&mut self, expected: Type, actual: Type) -> Result<Type, TypeError> {
        match (expected, actual) {
            // always choose the other one, it can never be less specific
            (Type::Var, other) | (other, Type::Var) => Ok(other),
            // Never is the supertype of all other types (except Any), so use the more specific subtype
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
                let inner = self.unify(expected_inner, actual_inner)?;
                Ok(Type::Ptr(inner))
            }
            (Type::Ptr(expected_inner), Type::MutPtr(actual_inner))
            | (Type::MutPtr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.unify(expected_inner, actual_inner)?;
                Ok(Type::MutPtr(inner))
            }
            (Type::Ptr(expected_inner), Type::ConstPtr(actual_inner))
            | (Type::ConstPtr(expected_inner), Type::Ptr(actual_inner)) => {
                let inner = self.unify(expected_inner, actual_inner)?;
                Ok(Type::ConstPtr(inner))
            }
            // mut pointers can be coerced to const pointers
            (Type::ConstPtr(expected_inner), Type::ConstPtr(actual_inner))
            | (Type::ConstPtr(expected_inner), Type::MutPtr(actual_inner))
            | (Type::MutPtr(expected_inner), Type::ConstPtr(actual_inner)) => {
                let inner = self.unify(expected_inner, actual_inner)?;
                Ok(Type::ConstPtr(inner))
            }
            // mut pointers are only equal to themselves (given equal pointees)
            (Type::MutPtr(expected_inner), Type::MutPtr(actual_inner)) => {
                let inner = self.unify(expected_inner, actual_inner)?;
                Ok(Type::MutPtr(inner))
            }
            // for generic types, unify the type parameters first
            (Type::Array(expected_inner, expected_len), Type::Array(actual_inner, actual_len)) => {
                if expected_len != actual_len {
                    unimplemented!();
                }

                let inner = self.unify(expected_inner, actual_inner)?;
                Ok(Type::Array(inner, expected_len))
            }
            (Type::Tuple(expected_inner), Type::Tuple(actual_inner)) => {
                if expected_inner.len() != actual_inner.len() {
                    unimplemented!();
                }

                let inner = expected_inner
                    .into_iter()
                    .zip(actual_inner)
                    .map(|(expected, actual)| self.unify(expected, actual))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Type::Tuple(inner))
            }
            (Type::Function(func), Type::Call(call)) | (Type::Call(call), Type::Function(func)) => {
                if func.params.len() != call.params.len() {
                    unimplemented!();
                }

                let mut params: Vec<_> = func.params.into_iter().enumerate().collect();
                let mut unified_params = Vec::with_capacity(params.len());

                // collect the named args first, since they determine the order of the unnamed args
                for arg in call.params.iter().filter(|param| param.name.is_some()) {
                    let (idx, mut param) =
                        support::find_remove(&mut params, |(_, param)| param.name == arg.name)
                            .unwrap_or_else(|| unimplemented!());

                    param.ty = self.unify(param.ty, arg.ty)?;
                    unified_params.push((idx, param));
                }

                // unify the remaining unnamed args from left to right
                for (arg, (idx, mut param)) in call
                    .params
                    .iter()
                    .filter(|param| param.name.is_none())
                    .zip(params)
                {
                    param.ty = self.unify(param.ty, arg.ty)?;
                    unified_params.push((idx, param));
                }

                // sort the params in the order of the definition, otherwise the type
                // of the function could have changed due to named arguments
                unified_params.sort_unstable_by_key(|&(idx, _)| idx);
                let unified_params: Vec<_> =
                    unified_params.into_iter().map(|(_, param)| param).collect();

                let ret = self.unify(func.ret, call.ret)?;

                Ok(Type::Function(Function {
                    params: unified_params,
                    ret,
                }))
            }
            // FIXME: this will currently unify functions with named parameters to ones without
            // that will cause defined functions to suddenly not be callable with named arguments

            // unify all params and the return type. For params, the names have to be
            // equal or not exist at all. If only one param name exists, it is coerced
            // to an unnamed parameter.
            (Type::Function(expected_fn), Type::Function(actual_fn)) => {
                if expected_fn.params.len() != actual_fn.params.len() {
                    unimplemented!();
                }

                let params = expected_fn
                    .params
                    .into_iter()
                    .zip(actual_fn.params)
                    .map(|(expected, actual)| {
                        let name = match (expected.name, actual.name) {
                            (Some(expected), Some(actual)) => {
                                if expected != actual {
                                    unimplemented!();
                                }

                                Some(expected)
                            }
                            (Some(_), None) | (None, Some(_)) => None,
                            (None, None) => None,
                        };

                        let ty = self.unify(expected.ty, actual.ty)?;
                        Ok(Param { name, ty })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let ret = self.unify(expected_fn.ret, actual_fn.ret)?;

                Ok(Type::Function(Function { params, ret }))
            }
            // nominal types are always fully known, so they can be compared directly
            (Type::Record(expected_record), Type::Record(actual_record)) => {
                if expected_record == actual_record {
                    Ok(Type::Record(expected_record))
                } else {
                    unimplemented!()
                }
            }
            (Type::Variants(expected_variants), Type::Variants(actual_variants)) => {
                if expected_variants == actual_variants {
                    Ok(Type::Variants(expected_variants))
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }
}
