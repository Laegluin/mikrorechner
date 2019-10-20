from tests import test
import datastrings as datastr

#wir müssen wg labels nur auf richtige datenstringform testen
def test_get_datastring_indexes():
    test.assertEquals(datastr.get_datastring_indexes(['R4 = R5 + R6']), [])
    test.assertEquals(datastr.get_datastring_indexes(['R4 = R5 + R6',
                                                  '0xFF']), [1])
    test.assertEquals(datastr.get_datastring_indexes(['R4 = R5 + R6',
                                                  '0b101011']), [1])
    test.assertEquals(datastr.get_datastring_indexes(['R4 = R5 + R6',
                                                  '124']), [1])
    test.assertEquals(datastr.get_datastring_indexes(['R4 = R5 + R6',
                                                  '-124']), [1])
    test.assertEquals(datastr.get_datastring_indexes(['0xFf',
                                                  '-124']), [0,1])
    test.assertEquals(datastr.get_datastring_indexes(['-0xFf',
                                                  '-124']), [1])

# def test_datastring_to_binary_string():
#     test.assertEquals(asm.datastring_to_binary_string('0xFFFFFF'),'1'*(3*8))
#     test.assertEquals(asm.datastring_to_binary_string('0x0'), '0' * 8)
#     test.assertEquals(asm.datastring_to_binary_string('0b111'), '0'*5+'1'*3)
#     test.assertEquals(asm.datastring_to_binary_string('255'), '1' * 8)
#     test.assertEquals(asm.datastring_to_binary_string('0b11111'), '0'*3+'1'*5)

def test_all():
    test_get_datastring_indexes()