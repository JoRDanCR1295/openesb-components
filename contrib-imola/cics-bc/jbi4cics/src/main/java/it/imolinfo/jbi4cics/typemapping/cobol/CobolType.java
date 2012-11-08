/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.typemapping.cobol;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Helper class to manage Cobol types. This class is used by
 * {@link CobolTypeDescriptor} to complete its functionalities.
 * <p>
 *
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public enum CobolType {

    // Es PIC X(n)
    STRING {
        Class getPreferredJavaType(CobolTypeDescriptor desc) {
            return String.class;
        }

        int getBufferedLength(CobolTypeDescriptor desc) throws FormatException {
            return desc.getStringLength();
        }

        void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                    int startingOffset) throws FormatException {
            if (value == null) {

                // Gestione del null, siamo in fase di input.
                // Dato che non e' possibile distinguere fra valori di input e
                // di output in una copy vanno inseriti dei valori di default
                // perche' il formatter si aspetta qualcosa cobol
                value = "";
            } else if (!(value instanceof String)) {
                LOG.error("CIC002104_String_not_found", value.getClass());
                throw new FormatException("CIC002104_String_not_found",
                                          new Object[] { value.getClass() });
            }
            StringMarshaller.marshallStringIntoBuffer((String) value, buffer,
                    startingOffset, desc.getCodePage(),
                    desc.getBufferedLength(), desc.getJustification(),
                    desc.getPadCharacter());
        }

        Object unformat(byte[] buffer, CobolTypeDescriptor desc,
                        int startingOffset) throws FormatException {
            return StringMarshaller.unmarshallString(buffer, startingOffset,
                    desc.getCodePage(), desc.getBufferedLength()).trim();
        }
    },

    // Es: PIC 9(n) comp o comp-4
    INTEGER {
        Class getPreferredJavaType(CobolTypeDescriptor desc) {
            return BigInteger.class;
        }

        int getBufferedLength(CobolTypeDescriptor desc) throws FormatException {
            int length = desc.getIntegerPartLength();

            if ((length < 0) || (length >= BUFFERED_LENGTH.length)) {
                LOG.error("CIC002109_Integer_too_much_long", length);
                throw new FormatException("CIC002109_Integer_too_much_long",
                                          new Object[] { length });
            }
            return BUFFERED_LENGTH[length];
        }

        void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                    int startingOffset) throws FormatException {
            if (value == null) {

                // Gestione del null, siamo in fase di input.
                // Dato che non e' possibile distinguere fra valori di input e
                // di output in una copy vanno inseriti dei valori di default
                // perche' il formatter si aspetta qualcosa cobol
                value = BigInteger.ZERO;
            }
            CobolFieldFormatter.formatInteger(value, buffer, desc,
                                              startingOffset);
        }

        Object unformat(byte[] buffer, CobolTypeDescriptor desc,
                        int startingOffset) throws FormatException {
            return IntegerMarshaller.unmarshallInteger(buffer, startingOffset,
                    desc.getBufferedLength(), desc.isBigEndian());
        }
    },

    // Es: PIC 9(n) comp-3 (con virgola sempre virtuale)
    PACKED_DECIMAL {
        Class getPreferredJavaType(CobolTypeDescriptor desc) {
            return BigDecimal.class;
        }

        int getBufferedLength(CobolTypeDescriptor desc) throws FormatException {
            return Math.round(((float) (desc.getIntegerPartLength()
                                       + desc.getDecimalPartLength() + 1)) / 2);
        }

        void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                    int startingOffset) throws FormatException {
            if (value == null){

                // Gestione del null, siamo in fase di input.
                // Dato che non e' possibile distinguere fra valori di input e
                // di output in una copy vanno inseriti dei valori di default
                // perche' il formatter si aspetta qualcosa cobol
                value = BigDecimal.ZERO;
            }
            CobolFieldFormatter.formatPackedDecimal(value, buffer, desc,
                                                    startingOffset);
        }

        Object unformat(byte[] buffer, CobolTypeDescriptor desc,
                        int startingOffset) throws FormatException {
            return PackedDecimalMarshaller.unmarshallBigDecimal(buffer, desc,
                                                                startingOffset);
        }
    },

    // Es: PIC 9(n) (con virgola virtuale o reale)
    ZONED {
        Class getPreferredJavaType(CobolTypeDescriptor desc) {
            return BigDecimal.class;
        }

        int getBufferedLength(CobolTypeDescriptor desc) throws FormatException {

            // TODO inserire la gestione di +9(n)...
            int length = desc.getIntegerPartLength()
                         + desc.getDecimalPartLength();

            if (desc.getZonedSignFormat()
                    == CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE) {
                length++;
            }
            return length;

        }

        void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                    int startingOffset) throws FormatException {
            if (value == null) {

                // Gestione del null, siamo in fase di input.
                // Dato che non e' possibile distinguere fra valori di input e
                // di output in una copy vanno inseriti dei valori di default
                // perche' il formatter si aspetta qualcosa cobol
                value = BigDecimal.ZERO;
            }
            CobolFieldFormatter.formatZoned(value, buffer, desc,
                                            startingOffset);
        }

        Object unformat(byte[] buffer, CobolTypeDescriptor desc,
                        int startingOffset) throws FormatException {
            return ZonedMarshaller.unmarshallZoned(buffer, startingOffset,
                    desc.getBufferedLength(), desc.isSigned(),
                    desc.getVirtualDecimalPoint(), desc.getZonedSignFormat(),
                    desc.getCodePage());
        }
    },

    // Tipo contenitore: 01 MAIN.
    NESTED_COMMAREA {
        Class getPreferredJavaType(CobolTypeDescriptor desc) {
            return desc.getNestedCommarea().getBeanClass();
        }

        int getBufferedLength(CobolTypeDescriptor desc) throws FormatException {
            return desc.getNestedCommarea().getBufferedLength();
        }

        
        void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                    int startingOffset) throws FormatException {
            if (value != null) {
                // MARCO (24/09/2008): with more the a "nested" child, the value can be null.
                CobolFieldFormatter.formatNestedCommarea(value, buffer, desc,
                                                     startingOffset);
            }

        }
        
        Object unformat(byte[] buffer, CobolTypeDescriptor desc,
            int startingOffset) throws FormatException {
            return CobolFieldFormatter.unformatNestedCommarea(buffer, desc,
                                                              startingOffset);
        }
        
    },

    /**
     * The <i>occurs</i> clause type.
     */
    OCCURS {
        Class getPreferredJavaType(CobolTypeDescriptor desc) {
            Class beanClass = desc.getNestedCommarea().getBeanClass();

            return Array.newInstance(beanClass, 0).getClass();
        }

        int getBufferedLength(CobolTypeDescriptor desc) throws FormatException {
            return desc.getNestedCommarea().getBufferedLength()
                   * desc.getOccursSize();
        }

        void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                    int startingOffset) throws FormatException {
            if (value == null) {

                // Gestione del null, siamo in fase di input.
                // Dato che non e' possibile distinguere fra valori di input e
                // di output in una copy vanno inseriti dei valori di default
                // perche' il formatter si aspetta qualcosa cobol
                throw new FormatException("CIC002101_Null_occurs_value");
            }
            CobolFieldFormatter.formatOccurs(value, buffer, desc,
                                             startingOffset);
        }

        Object unformat(byte[] buffer, CobolTypeDescriptor desc,
                        int startingOffset) throws FormatException {
            return CobolFieldFormatter.unformatOccurs(buffer, desc,
                                                      startingOffset);
        }
    };

    /**
     * Lookup table to obtain the integer buffered length from its integer part
     * length. The integer part length must be used as index of this array,
     * while the corresponding entry value is the associated buffered length.
     */
    private static final int[] BUFFERED_LENGTH = new int[] {

        // 1 byte fino a 256 o +- 128 --> 10^2
        // if (integerPartLength <= 2) return 1;
        1, 1, 1,

        // 2 byte fino a 65536 o +- 32768 --> 10^4
        // if (integerPartLength <= 4) return 2;
        2, 2,

        // 4 byte fino a 4294967296 o +- 2147483648 --> 10^9
        // if (integerPartLength <= 9) return 4;
        4, 4, 4, 4, 4,

        // 8 byte fino a 18446744073709551616 o +- 9223372036854775808 --> 10^18
        // if (integerPartLength <= 18) return 8;
        8, 8, 8, 8, 8, 8, 8, 8, 8,

        // 16 byte fino a 3.402823669e+38 o +- 1.701411835e+38 --> 10^37
        // if (integerPartLength <= 37) return 16;
        16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
        16
    };

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG = LoggerFactory.getLogger(CobolType.class);

    abstract Class getPreferredJavaType(CobolTypeDescriptor desc);

    abstract int getBufferedLength(CobolTypeDescriptor desc)
            throws FormatException;

    abstract void format(Object value, byte[] buffer, CobolTypeDescriptor desc,
                         int startingOffset) throws FormatException;

    abstract Object unformat(byte[] buffer, CobolTypeDescriptor desc,
                             int startingOffset) throws FormatException;
}
