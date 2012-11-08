/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/

package it.imolinfo.jbi4ejb.jbi.xfire;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.Jbi4EjbLifeCycle;

import java.io.OutputStream;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.XFireException;
import org.codehaus.xfire.exchange.OutMessage;
import org.codehaus.xfire.transport.AbstractChannel;
import org.codehaus.xfire.transport.Channel;

/**
 * Ejb channel, only support local invocations and backchannel uri.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class EjbChannel extends AbstractChannel {
    
    /** The logger for this class and its instances. */
    private static final Logger LOG = LoggerFactory.getLogger(EjbChannel.class);
    private static final Messages MESSAGES = Messages.getMessages(EjbChannel.class);
    
    /** The output factory. */
    private XMLOutputFactory outputFactory;             

    /**
     * Instantiates a new ejb channel.
     * 
     * @param uri the channel uri
     * @param transport the EjbTransport
     */
    public EjbChannel(String uri, EjbTransport transport) {
        setTransport(transport);
        setUri(uri);
        this.outputFactory = XMLOutputFactory.newInstance();        
    }

    
    /**
     * Do nothing..
     * 
     * @throws Exception if some proble occurs
     * 
     * @see org.codehaus.xfire.transport.Channel#open()
     */
    public void open() throws Exception {
        // Do nothing...
    }

    /* (non-Javadoc)

     */
    
    
    /**
     * Sends the message.
     * 
     * @param context The message context
     * @param message the out message
     * @see org.codehaus.xfire.transport.Channel#send(org.codehaus.xfire.MessageContext, org.codehaus.xfire.exchange.OutMessage)
     * @throws XFireException if some problem occurs 
     */
    public void send(MessageContext context, OutMessage message) throws XFireException {
        LOG.debug("Xfire send called");
        if (Channel.BACKCHANNEL_URI.equals(message.getUri())) {
            LOG.debug("message.getUri(): " + message.getUri());
            final OutputStream out = (OutputStream) context.getProperty(Channel.BACKCHANNEL_URI);
            if (out != null) {
                try {
                    final XMLStreamWriter writer = outputFactory.createXMLStreamWriter(out, message.getEncoding());                                        
                    message.getSerializer().writeMessage(message, writer, context);                    
                    writer.close();
                } catch (XMLStreamException e) {
                	String msg=MESSAGES.getString("EJB000501_EjbChannel", new Object[]{e.getMessage()});
                    LOG.error(msg,e);
                    throw new XFireException(msg,e);   

                }
                return;
            }
        } else {
            // Not implemented.         
            LOG.debug("NOT a backchannel URI, not implemented");
        }
    }

}
