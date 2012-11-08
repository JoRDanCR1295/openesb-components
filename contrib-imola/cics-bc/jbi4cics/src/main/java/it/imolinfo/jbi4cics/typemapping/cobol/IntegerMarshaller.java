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
import java.math.BigInteger;
import java.util.Arrays;

final class IntegerMarshaller {

    /**
     * The logger for this class.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(IntegerMarshaller.class);

    private IntegerMarshaller() {
    }

    static void marshallInteger(BigInteger value, byte[] buffer, int offset,
            int size, boolean isBigEndian) throws FormatException {
        byte[] array = value.toByteArray();
        int length = array.length;
        byte filler;

        if (LOG.isDebugEnabled()) {
            LOG.debug("value.toString: " + value);
        }
        if (length > size) {
            Object[] args = new Object[] { size, value, length };

            LOG.error("CIC002114_Unexpected_size", args);
            throw new FormatException("CIC002114_Unexpected_size", args);
        }
        if (value.signum() == -1) {
            filler = (byte) 0xff;                       // complemento a 2
        } else {
            filler = 0;
        }
        if (isBigEndian) {
            System.arraycopy(array, 0, buffer, offset + size - length, length);
            Arrays.fill(buffer, offset, offset + size - length, filler);
        } else {
            array = reverse(array);
            System.arraycopy(array, 0, buffer, offset, length);
            Arrays.fill(buffer, offset + length, offset + size - length, filler);
        }
    }

    static BigInteger unmarshallInteger(byte[] buffer, int offset, int size, 
                                        boolean isBigEndian) {
        byte[] array = new byte[size];

        System.arraycopy(buffer, offset, array, 0, size);
        if (!isBigEndian) {
            array = reverse(array);
        }
        return new BigInteger(array);
    }

    private static byte[] reverse(byte[] array) {
        int length = array.length;
        byte[] result = new byte[length];

        for (int i = length - 1; i >= 0; --i) {
            result[i] = array[length - i - 1];
        }
        return result;
    }
}
