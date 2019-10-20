from tests import test
import labels as lb

def test_is_datastring():
    test.assertTrue(lb.is_datastring('0xFfA'))
    test.assertTrue(lb.is_datastring('-123645'))
    test.assertTrue(lb.is_datastring('235'))
    test.assertTrue(lb.is_datastring('0b0101001'))
    test.assertFalse(lb.is_datastring('0xFfg'))
    test.assertFalse(lb.is_datastring('-0xFfA'))
    test.assertFalse(lb.is_datastring('-0b1'))
    test.assertFalse(lb.is_datastring(''))

def test_necessary_byte_storage():
    test.assertEquals(lb.necessary_byte_storage('R4 = R5 + R6'), 4)
    test.assertEquals(lb.necessary_byte_storage('\t'), 0)
    test.assertEquals(lb.necessary_byte_storage('0xFFFFFF'), 3)
    test.assertEquals(lb.necessary_byte_storage('0'), 1)
    test.assertEquals(lb.necessary_byte_storage('256'), 2)
    test.assertEquals(lb.necessary_byte_storage('255'), 1)

def test_cut_labels():
    test.assertEquals(lb.cut_labels(['hallo welt _sdf']), ['hallo welt'])
    test.assertEquals(lb.cut_labels(['hallo welt_sdf']), ['hallo welt_sdf'])
    test.assertEquals(lb.cut_labels(['hallo_welt _sdf']), ['hallo_welt'])
    test.assertEquals(lb.cut_labels(['hallo welt _sdf usw ']), ['hallo welt _sdf usw '])
    test.assertEquals(lb.cut_labels(['hallo welt _??']), ['hallo welt _??'])
    test.assertEquals(lb.cut_labels(['']), [''])

def test_cut_comments():
    test.assertEquals(lb.cut_comments(['hallo welt _sdf #hier erst cutten']), ['hallo welt _sdf '])
    test.assertEquals(lb.cut_comments(['hallo welt _sdf#hier erst cutten']), ['hallo welt _sdf'])
    test.assertEquals(lb.cut_comments(['hallo welt #hier erst cutten _keine_labels_mitzählen']), ['hallo welt '])
    test.assertEquals(lb.cut_comments(['#hallo welt _sdf #hier erst cutten']), [''])

def test_cut_whitespace_lines():
    test.assertEquals(lb.cut_whitespace_lines(['']), [])
    test.assertEquals(lb.cut_whitespace_lines(['\t', 'a']), ['a'])
    test.assertEquals(lb.cut_whitespace_lines(['\n']), [])
    test.assertEquals(lb.cut_whitespace_lines(['hallo', '', 'hallo']), ['hallo', 'hallo'])


def test_get_label_values_dictionary():
    test.assertEquals(lb.get_label_values_dictionary(['R4 = R5 _label']), {'label': 'R4 = R5 '})
    test.assertEquals(lb.get_label_values_dictionary(['R4 = R5 _label #hier nicht']), {})
    test.assertEquals(lb.get_label_values_dictionary([' _label']), {'label': ' '})
    test.assertEquals(lb.get_label_values_dictionary(['R4 = R5 _?']), {})

def test_all():
    test_is_datastring()
    test_necessary_byte_storage()
    test_cut_labels()
    test_cut_comments()
    test_get_label_values_dictionary()
    test_cut_whitespace_lines()


