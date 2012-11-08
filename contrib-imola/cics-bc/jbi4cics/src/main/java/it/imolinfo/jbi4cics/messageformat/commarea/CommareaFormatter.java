/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.messageformat.commarea;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;
import it.imolinfo.jbi4cics.messageformat.MessageFormatter;
import it.imolinfo.jbi4cics.service.ServiceContext;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolFieldFormatter;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;
import it.imolinfo.jbi4cics.typemapping.cobol.HexDump;
import java.util.Map;
import org.apache.commons.beanutils.ConvertingWrapDynaBean;
import org.apache.commons.beanutils.WrapDynaBean;

/**
 * Questa classe serve a formattare una commarea secondo quanto definito nel
 * CommareaBeanMappingDescriptor known limitation: ad oggi non sono supportati i
 * filler e i nested bean known bugs: ci sono dei problemi di conversione da e
 * verso Float e Double (temo ineliminabili perche' dovuti alla limitata
 * precisione del formato dati) c'è un problema di conversione con BigDecimal
 * non meglio identificato, vedere SimpleCommareaMappingTest.
 *
 * @author raffaele
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class CommareaFormatter implements MessageFormatter {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(CommareaFormatter.class);

    /**
     * Initializes a new instance of this class.
     */
    public CommareaFormatter() {
    }

    public void mapInputBeanToInputMessage(ServiceContext context)
            throws FormatException {
        MappingDescriptor desc = context.getInputMappingDescriptor();
        WrapDynaBean bean = new WrapDynaBean(context.getInputBean());
        byte[] buffer;
        int offset = 0;

        // If we're here then we must have a CommareaBeanMappingDescriptor
        if (!(desc instanceof CommareaBeanMappingDescriptor)) {
            LOG.error("CIC001702_Expected_commarea_bean_mapping_descriptor",
                      desc.getClass());
            throw new FormatException(
                    "CIC001702_Expected_commarea_bean_mapping_descriptor",
                    new Object[] { desc.getClass() });
        }
        buffer = new byte[
                ((CommareaBeanMappingDescriptor) desc).getBufferedLength()];

        for (Map.Entry<String, FieldDescriptor> entry
                : desc.getFieldMap().entrySet()) {
            CobolTypeDescriptor ctDesc = (CobolTypeDescriptor) entry.getValue();
            String propertyName = entry.getKey();
            Object value = bean.get(propertyName);

            CobolFieldFormatter.format(value, buffer, ctDesc, offset);
            offset += ctDesc.getBufferedLength();
        }
        context.setInputMessage(buffer);

        if (LOG.isDebugEnabled()) {
            LOG.debug("formatted input bean: [" + context.getInputBean()
                      + "]\nin input buffer: " + HexDump.toHex(buffer)
                      + "\nformatted input bean: [" + context.getInputBean()
                      + "]\nin input buffer: [" + HexDump.dump(buffer, 0, 0)
                      + "]");
        }
    }

    public void mapOutputMessageToOutputBean(ServiceContext context)
            throws FormatException {
        MappingDescriptor desc = context.getOutputMappingDescriptor();
        Object outputMessage = context.getOutputMessage();
        WrapDynaBean bean;
        byte[] buffer;
        int offset = 0;

        // If we're here then we must have a CommareaBeanMappingDescriptor
        if (!(desc instanceof CommareaBeanMappingDescriptor)) {
            LOG.error("CIC001702_Expected_commarea_bean_mapping_descriptor",
                      desc.getClass());
            throw new FormatException(
                    "CIC001702_Expected_commarea_bean_mapping_descriptor",
                    new Object[] { desc.getClass() });
        }

        if (!(outputMessage instanceof byte[])) {
            Object[] args = { outputMessage.getClass() };

            LOG.error("CIC001703_Expected_byte_array", args);
            throw new FormatException("CIC001703_Expected_byte_array", args);
        }
        buffer = (byte[]) outputMessage;

        try {
            Object instance = desc.getBeanClass().newInstance();

            bean = new ConvertingWrapDynaBean(instance);
        } catch (IllegalAccessException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw new FormatException(e);
        } catch (InstantiationException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw new FormatException(e);
        }
        for (Map.Entry<String, FieldDescriptor> entry
                : desc.getFieldMap().entrySet()) {
            CobolTypeDescriptor ctDesc = (CobolTypeDescriptor) entry.getValue();
            String propertyName = entry.getKey();
            Object value = CobolFieldFormatter.unformat(buffer, ctDesc, offset);

            bean.set(propertyName, value);
            offset += ctDesc.getBufferedLength();
        }
        context.setOutputBean(bean.getInstance());

        if (LOG.isDebugEnabled()) {
            LOG.debug("formatted output buffer: [" + HexDump.toHex(buffer)
                      + "]\nin output bean: " + context.getOutputBean()
                      + "\nformatted output buffer: ["
                      + HexDump.dump(buffer, 0, 0) + "]\nin output bean: "
                      + context.getOutputBean());
        }
    }
}
