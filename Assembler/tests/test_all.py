from tests import test_labels_beta, test_assemble_data_strings, test_asm_beta


def test_all():
    print('Teste asm_beta')
    test_asm_beta.test_all()
    print('\n')
    print('Teste asm_labels')
    test_labels_beta.test_all()
    print('\n')
    print('teste assemble_data_strings')
    test_assemble_data_strings.test_all()
