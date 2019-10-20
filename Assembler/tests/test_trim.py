import test
import trim

def test_all():
    test_cut_whitespace_lines()
    test_cut_comments()
    test_cut_labels()
    test_trim_before_label_module()
    test_trim_word_gap()

def test_cut_comments():
    test.assertEquals(trim.cut_comments(['hallo #sdfe']),['hallo '])
    test.assertEquals(trim.cut_comments(['hallo #sdfe','wie gehts #sdf']), ['hallo ','wie gehts '])
    test.assertEquals(trim.cut_comments(['hallo welt _sdf #hier erst cutten']), ['hallo welt _sdf '])
    test.assertEquals(trim.cut_comments(['hallo welt _sdf#hier erst cutten']), ['hallo welt _sdf'])
    test.assertEquals(trim.cut_comments(['hallo welt #hier erst cutten _keine_labels_mitzählen']), ['hallo welt '])
    test.assertEquals(trim.cut_comments(['#hallo welt _sdf #hier erst cutten']), [''])

def test_cut_whitespace_lines():
    test.assertEquals(trim.cut_whitespace_lines(['hallo','\n','naa']),['hallo','naa'])
    test.assertEquals(trim.cut_whitespace_lines(['']), [])
    test.assertEquals(trim.cut_whitespace_lines(['\t', 'a']), ['a'])
    test.assertEquals(trim.cut_whitespace_lines(['\n']), [])
    test.assertEquals(trim.cut_whitespace_lines(['hallo', '', 'hallo']), ['hallo', 'hallo'])

def test_trim_word_gap():
    test.assertEquals(trim.trim_word_gap(['hallo\t','\n','naa']),['hallo ',' ','naa'])

def test_cut_labels():
    test.assertEquals(trim.cut_labels(['hallo welt_sdf']), ['hallo welt_sdf'])
    test.assertEquals(trim.cut_labels(['hallo_welt _sdf']), ['hallo_welt'])
    test.assertEquals(trim.cut_labels(['hallo welt _sdf usw ']), ['hallo welt _sdf usw '])
    test.assertEquals(trim.cut_labels(['hallo welt _??']), ['hallo welt _??'])
    test.assertEquals(trim.cut_labels(['']), [''])

def test_trim_before_label_module(): pass


