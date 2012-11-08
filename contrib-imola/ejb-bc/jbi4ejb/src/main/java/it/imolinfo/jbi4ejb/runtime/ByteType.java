/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.runtime;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import javax.xml.namespace.QName;

import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.aegis.MessageReader;
import org.codehaus.xfire.aegis.MessageWriter;
import org.codehaus.xfire.aegis.type.Type;
import org.codehaus.xfire.fault.XFireFault;

/**
 * This class is used to customize the 'byte' data type (java) mapping in XFire.
 */
public class ByteType extends Type {

    /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ByteType.class);

    /**
    * Empty Constructor.
    *
    * Setting up the XML Schema.
    */
    public ByteType() {
        setNillable(true);
        setSchemaType(new QName("http://www.w3.org/2001/XMLSchema","byte"));
        setTypeClass(byte.class);
    }

    
    /**
     * Reads a ByteType object.
     * 
     * @param reader
     *            The reader
     * @param context
     *            The MessageContext
     * 
     * @return
     *          The read object
     * 
     * @throws XFireFault
     *             If some problem occurs
     * 
     * The readen object
     * @see org.codehaus.xfire.aegis.type.Type#readObject(org.codehaus.xfire.aegis.MessageReader,
     *      org.codehaus.xfire.MessageContext)
     */
    public Object readObject(MessageReader reader, MessageContext context)
        throws XFireFault {

        LOG.debug(">>>>> ByteType.readObject - begin:" + reader.getValue());

        String value = reader.getValue();

        if (value == null || "".equals(value)){
        LOG.debug("<<<<< ByteType.readObject - end: the value is null.");
        return null;
        }

        LOG.debug("<<<<< ByteType.readObject - end: the value is " + value);
        return new Byte(value);
    }

    
    /**
     * Writes out the ByteType object.
     * 
     * @param object
     *          The object 
     * @param writer
     *          The writer
     * @param context
     *          The MessageContext
     * 
     * @throws XFireFault
     *              If some problem occurs
     * 
     * @see org.codehaus.xfire.aegis.type.Type#writeObject(java.lang.Object,
     *      org.codehaus.xfire.aegis.MessageWriter,
     *      org.codehaus.xfire.MessageContext)
     */
    public void writeObject(Object object,
                            MessageWriter writer,
                            MessageContext context) throws XFireFault {
        String objectString = "";
        if (object != null) {
            objectString = object.toString();
        }
        
        if (LOG.isDebugEnabled()) {
            String debugMsg = ">>>>> ByteType.writeObject - begin: object=" + object
            + "; class=" + objectString;
            LOG.debug(debugMsg);
        }
        
        writer.writeValue(objectString);

        LOG.debug("<<<<< ByteType.writeObject - end");
    }

}
