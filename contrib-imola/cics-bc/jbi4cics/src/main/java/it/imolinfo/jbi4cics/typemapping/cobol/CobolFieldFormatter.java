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
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaFormatter;
import it.imolinfo.jbi4cics.service.ServiceContext;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Questo metodo formatta un tipo java in un buffer di byte secondo il formato
 * cobol. Il metodo ha i seguenti vincoli: Tipo cobol STRING l'oggetto passato
 * deve essere di tipo String Tipo cobol PACKED_DECIMAL l'oggetto passato deve
 * essere BigInteger o BigDecimal o Long o Integer o Float o Double Tipo cobol
 * INTEGER l'oggetto passato deve essere di tipo Number, se la dimensione e' >
 * 16 ci possono essere problemi di conversione Tipo cobol ZONED l'oggetto
 * passato deve essere BigInteger o BigDecimal o Long o Integer o Float o Double
 * l'invariante di questo metodo e' che alla fine lo spazio occupato sull'array
 * sara' sempre esattamente cobolType.getBufferedSize()
 * TODO ATTENZIONE su questo punto vanno fatte maggiori verifiche (ad esempio
 * come comportarsi nel caso una stringa sia troppo lunga...)
 *
 * @author raffaele
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class CobolFieldFormatter {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(CobolFieldFormatter.class);

    /**
     * Does nothing. This constructor is declared <code>private</code> to
     * prevent the instances creation of this class.
     */
    private CobolFieldFormatter() {
    }

    public static void format(Object value, byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        CobolType ct = desc.getType();

        if (ct == null) {
            LOG.error("CIC002102_Unrecognized_cobol_type", ct);
            throw new FormatException("CIC002102_Unrecognized_cobol_type",
                                      new Object[] { ct });
        }
        if ((buffer.length - offset) < desc.getBufferedLength()) {
            Object[] args = { buffer.length, offset, desc.getBufferedLength() };

            LOG.error("CIC002103_Not_enough_space_writing_cobol_type", args);
            throw new FormatException("CIC002103_Not_enough_space", args);
        }
        ct.format(value, buffer, desc, offset);
    }

    static void formatOccurs(Object value, byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        Object[] valueArray;
        boolean debug = LOG.isDebugEnabled();

        if (!(value instanceof Object[])) {
            Object[] args = { value.getClass() };

            LOG.error("CIC002105_Array_not_found", args);
            throw new FormatException("CIC002105_Array_not_found", args);
        }
        valueArray = (Object[]) value;
        if (valueArray.length > desc.getOccursSize()) {
            Object[] args = { valueArray.length, desc.getOccursSize() };

            LOG.error("CIC002106_Array_size_not_allowed", args);
            throw new FormatException("CIC002106_Array_size_not_allowed", args);
        }
        for (int i = 0; i < valueArray.length; i++) {

            // questo serve a riutilizzare quanto scritto per
            // formatNestedCommarea, costruisce in cobolTypeNested finto
            CobolTypeDescriptor nestedDesc = new CobolTypeDescriptor();
            CommareaBeanMappingDescriptor nested = desc.getNestedCommarea();

            if (debug) {
                LOG.debug("buffer.length: " + buffer.length + " index i: " + i
                          + " startingOffset: " + offset);
            }

            nestedDesc.setType(CobolType.NESTED_COMMAREA);
            nestedDesc.setNestedCommarea(nested);
            formatNestedCommarea(valueArray[i], buffer, nestedDesc, offset);
            offset += nested.getBufferedLength();
        }
    }

    static void formatNestedCommarea(Object value, byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        ServiceContext ctx = new ServiceContext();

        ctx.setInputBean(value);
        ctx.setInputMappingDescriptor(desc.getNestedCommarea());
        new CommareaFormatter().mapInputBeanToInputMessage(ctx);
        System.arraycopy(ctx.getInputMessage(), 0, buffer, offset,
                         desc.getBufferedLength());
    }

    static void formatInteger(Object value, byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        if (value instanceof Number) {
            String valueToString = value.toString();
            BigInteger bigIntegerValue;

            if (LOG.isDebugEnabled()) {
                LOG.debug("numberValue.toString(): " + valueToString);
            }
            bigIntegerValue = new BigDecimal(valueToString).toBigIntegerExact();
            IntegerMarshaller.marshallInteger(bigIntegerValue, buffer,
                    offset, desc.getBufferedLength(), desc.isBigEndian());
        } else {
            Object[] args = { value.getClass() };

            LOG.error("CIC002107_Number_not_found", args);
            throw new FormatException("CIC002107_Number_not_found", args);
        }
    }

    static void formatPackedDecimal(Object value, byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        if (value instanceof Number) {
            String valueToString = value.toString();
            BigDecimal bigDecimalValue;

            if (LOG.isDebugEnabled()) {
                LOG.debug("numberValue.toString(): " + valueToString);
            }
            bigDecimalValue = new BigDecimal(valueToString);
            PackedDecimalMarshaller.marshallBigDecimal(bigDecimalValue, buffer,
                                                       desc, offset);
        } else {
            Object[] args = { value.getClass() };

            LOG.error("CIC002107_Number_not_found", args);
            throw new FormatException("CIC002107_Number_not_found", args);
        }
    }

    static void formatZoned(Object value, byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        if (value instanceof Number) {
            String valueToString = value.toString();
            BigDecimal bigDecimalValue;

            if (LOG.isDebugEnabled()) {
                LOG.debug("numberValue.toString(): " + valueToString);
            }
            bigDecimalValue = new BigDecimal(valueToString);
            ZonedMarshaller.marshallZoned(bigDecimalValue, buffer,
                    offset, desc.getBufferedLength(), desc.isSigned(),
                    desc.getVirtualDecimalPoint(), desc.getZonedSignFormat(),
                    desc.getCodePage());
        } else {
            Object[] args = { value.getClass() };

            LOG.error("CIC002107_Number_not_found", args);
            throw new FormatException("CIC002107_Number_not_found", args);
        }
    }

    /**
     * Questo metodo legge un tipo cobol descritto da cobolType nel buffer a
     * partire da startingOffset e crea il corrispondente tipo java le
     * corrispondenze sono le seguenti: tipo cobol STRING tipo java String tipo
     * cobol PACKED_DECIMAL tipo java BigDecimal tipo cobol INTEGER tipo java
     * BigInteger tipo cobol ZONED tipo java BigDecimal
     *
     * @param buffer
     * @param desc
     * @param offset
     * @return
     * @throws FormatException
     */
    public static Object unformat(byte[] buffer, CobolTypeDescriptor desc,
            int offset) throws FormatException {
        CobolType cobolType = desc.getType();

        if ((buffer.length - offset) < desc.getBufferedLength()) {
            Object[] args = { buffer.length, offset, desc.getBufferedLength() };

            LOG.error("CIC002108_Not_enough_space_reading_cobol_type", args);
            throw new FormatException(
                    "CIC002108_Not_enough_space_reading_cobol_type", args);
        }
        if (cobolType == null) {
            LOG.error("CIC002102_Unrecognized_cobol_type", cobolType);
            throw new FormatException("CIC002102_Unrecognized_cobol_type",
                                      new Object[] { cobolType });
        }
        return cobolType.unformat(buffer, desc, offset);
    }

    static Object unformatOccurs(byte[] buffer, CobolTypeDescriptor desc,
            int offset) throws FormatException {
        CommareaBeanMappingDescriptor nested = desc.getNestedCommarea();
        Object[] value = (Object[]) Array.newInstance(nested.getBeanClass(),
                                                      desc.getOccursSize());

        for (int i = 0; i < value.length; i++) {

            // questo serve a riutilizzare quanto scritto per
            // formatNestedCommarea, costruisce in cobolTypeNested finto
            CobolTypeDescriptor nestedDesc = new CobolTypeDescriptor();

            nestedDesc.setType(CobolType.NESTED_COMMAREA);
            nestedDesc.setNestedCommarea(nested);
            value[i] = unformatNestedCommarea(buffer, nestedDesc, offset);
            offset += nested.getBufferedLength();
        }
        return value;
    }

    static Object unformatNestedCommarea(byte[] buffer,
            CobolTypeDescriptor desc, int offset) throws FormatException {
        ServiceContext ctx = new ServiceContext();
        byte[] outputMessage = new byte[desc.getBufferedLength()];

        System.arraycopy(buffer, offset, outputMessage, 0,
                         outputMessage.length);
        ctx.setOutputMessage(outputMessage);
        ctx.setOutputMappingDescriptor(desc.getNestedCommarea());
        new CommareaFormatter().mapOutputMessageToOutputBean(ctx);
        return ctx.getOutputBean();
    }
}
