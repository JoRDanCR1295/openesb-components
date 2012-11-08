/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.io.OutputStream;
import java.lang.reflect.Array;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLEventWriter;
import javax.xml.stream.XMLStreamWriter;

import org.apache.cxf.common.logging.LogUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.service.model.MessagePartInfo;
import org.w3c.dom.Node;

/**
 * Helper class for the jaxb mapping of the returned results.
 * Derived from the JxbEncoderDecoder class.
 * 
 * @author mpiraccini@imolinfo.it
 *
 */
public final class OutMarshallerHelper {

    /**
     * Logger.
     */
    private static final transient Logger LOG = LoggerFactory.getLogger(OutMarshallerHelper.class);
    /** The messages. */
    @SuppressWarnings("unused")
    private static final Messages MESSAGES =
            Messages.getMessages(OutMarshallerHelper.class);
    /** The CXF Logger (needed by the Fault constructors) */
    private static final java.util.logging.Logger CXFLOG = LogUtils.getLogger(Jbi4CorbaBareOutInterceptor.class);

    /**
     * Writes out the object on the marshaler.
     * @param u
     * @param source
     * @param mObj
     * @throws Fault
     * @throws JAXBException
     */
    public static void writeObject(Marshaller u, Object source, Object mObj)
            throws Fault, JAXBException {
        if (source instanceof XMLStreamWriter) {
            u.marshal(mObj, (XMLStreamWriter) source);
        } else if (source instanceof OutputStream) {
            u.marshal(mObj, (OutputStream) source);
        } else if (source instanceof Node) {
            u.marshal(mObj, (Node) source);
        } else if (source instanceof XMLEventWriter) {
            u.marshal(mObj, (XMLEventWriter) source);
        } else {
            throw new Fault(new org.apache.cxf.common.i18n.Message(
                    "UNKNOWN_SOURCE", CXFLOG, source.getClass().getName()));
        }
    }

    /**
     * Marshal an object using jabx.
     * This implementation supports array as return values.
     * @param marshaller
     * @param elValue
     * @param part
     * @param source
     * @throws Fault
     */
    @SuppressWarnings("unchecked")
    public static void marshal(Marshaller marshaller, Object elValue,
            MessagePartInfo part, Object source) throws Fault {

        try {
            // The Marshaller.JAXB_FRAGMENT will tell the Marshaller not to
            // generate the xml declaration.
            marshaller.setProperty(Marshaller.JAXB_FRAGMENT, true);
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, false);
        } catch (javax.xml.bind.PropertyException e) {
            // intentionally empty.
            LOG.debug(e.getMessage());
        }

        Class<?> cls = null;
        if (part != null) {
            cls = part.getTypeClass();
        }

        if (cls == null) {
            cls = null != elValue ? elValue.getClass() : null;
        }

        if (cls != null && cls.isArray() && elValue instanceof Collection) {
            Collection<?> col = (Collection<?>) elValue;
            elValue = col.toArray((Object[]) Array.newInstance(cls.getComponentType(), col.size()));
        }

        try {
            Object mObj = elValue;
            QName elName = null;
            if (part != null) {
                elName = part.getConcreteName();
            }

            if (null != elName) {

                if (part != null) {
                    //	&& part.getXmlSchema() instanceof XmlSchemaElement) {

                    //XmlSchemaElement el = (XmlSchemaElement) part
                    //		.getXmlSchema();

                    //if (mObj.getClass().isArray()
                    // && el.getSchemaType() instanceof XmlSchemaSimpleType
                    // && part.getXmlSchema() instanceof XmlSchemaSimpleType
                    //&& ((XmlSchemaSimpleType) el.getSchemaType())
                    //		.getContent() instanceof XmlSchemaSimpleTypeList
                    // ) {
                    // mObj = Arrays.asList((Object[]) mObj);
                    // writeObject(marshaller, source, M);
                    if (mObj == null) {
                        Class clazz = part.getTypeClass();
                        LOG.debug("null value deducting type class: "+clazz.toString());
//                        OutMarshallerHelper.writeObject(marshaller, source,
//                                new JAXBElement(elName, Array.newInstance(Class.forName("AlarmIRPConstDefs.BadAcknowledgeAlarmInfo", true, Thread.currentThread().getContextClassLoader()), 0).getClass(), null));
                        OutMarshallerHelper.writeObject(marshaller, source, new JAXBElement(elName, clazz, null));


                        return;
                    }


                    if (mObj.getClass().isArray()) {
                        // Have to handle this ourselves.... which really
                        // sucks.... but what can we do?
                        Object objArray;
                        if (mObj instanceof List) {
                            List l = (List) mObj;
                            objArray = l.toArray(new Object[l.size()]);
                            cls = null;
                        } else {
                            objArray = mObj;
                            cls = objArray.getClass().getComponentType();
                        }
                        int len = Array.getLength(objArray);
                        for (int x = 0; x < len; x++) {
                            Object o = Array.get(objArray, x);
                            OutMarshallerHelper.writeObject(marshaller, source,
                                    new JAXBElement(elName, cls == null ? o.getClass() : cls, o));
                        }
                    } else {
                        OutMarshallerHelper.writeObject(marshaller, source, new JAXBElement(elName,
                                cls, mObj));
                    }
                } else if (byte[].class == cls && part.getTypeQName() != null && part.getTypeQName().getLocalPart().equals(
                        "hexBinary")) {
                    mObj = new HexBinaryAdapter().marshal((byte[]) mObj);
                    OutMarshallerHelper.writeObject(marshaller, source, new JAXBElement(elName,
                            String.class, mObj));
                } else if (mObj instanceof JAXBElement) {
                    OutMarshallerHelper.writeObject(marshaller, source, mObj);
                } else {
                    OutMarshallerHelper.writeObject(marshaller, source, new JAXBElement(elName,
                            cls, mObj));
                }
            } else {
                OutMarshallerHelper.writeObject(marshaller, source, mObj);
            }
        } catch (Fault ex) {
            ex.printStackTrace();
            throw (Fault) ex.fillInStackTrace();
        } catch (Exception ex) {
            ex.printStackTrace();
            if (ex instanceof javax.xml.bind.MarshalException) {
                javax.xml.bind.MarshalException marshalEx = (javax.xml.bind.MarshalException) ex;
                org.apache.cxf.common.i18n.Message faultMessage = new org.apache.cxf.common.i18n.Message("MARSHAL_ERROR", CXFLOG, marshalEx.getLinkedException().getMessage());
                throw new Fault(faultMessage, ex);
            } else {
                throw new Fault(new org.apache.cxf.common.i18n.Message("MARSHAL_ERROR", CXFLOG, ex.getMessage()), ex);
            }
        }
    }
}
