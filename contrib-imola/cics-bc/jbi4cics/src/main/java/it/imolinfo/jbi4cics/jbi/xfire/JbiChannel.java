/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.jbi.xfire;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.jbi.Messages;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.apache.servicemix.jbi.jaxp.StAXSourceTransformer;
import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.XFireException;
import org.codehaus.xfire.exchange.InMessage;
import org.codehaus.xfire.exchange.MessageSerializer;
import org.codehaus.xfire.exchange.OutMessage;
import org.codehaus.xfire.fault.XFireFault;
import org.codehaus.xfire.soap.AbstractSoapBinding;
import org.codehaus.xfire.transport.AbstractChannel;
import org.codehaus.xfire.transport.Channel;

/**
 * Jbi channel, only support local invocations. 
 */
public class JbiChannel extends AbstractChannel {

    public static final String JBI_INTERFACE_NAME = "jbi.interface";
    public static final String JBI_SERVICE_NAME = "jbi.service";
    public static final String JBI_ENDPOINT = "jbi.endpoint";
    
    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(JbiChannel.class);    
    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(JbiChannel.class);   
    
    private StAXSourceTransformer sourceTransformer;
    private XMLOutputFactory outputFactory;
 
    
    public JbiChannel(String uri, JbiTransport transport) {
        setTransport(transport);
        setUri(uri);
        this.sourceTransformer = new StAXSourceTransformer();
        this.outputFactory = XMLOutputFactory.newInstance();
    }

    public void open() throws Exception {
    }

    public void send(MessageContext context, OutMessage message) throws XFireException {
        if (Channel.BACKCHANNEL_URI.equals(message.getUri())) {
            final OutputStream out = (OutputStream) context.getProperty(Channel.BACKCHANNEL_URI);
            if (out != null) {
                try {
                    final XMLStreamWriter writer = outputFactory.createXMLStreamWriter(out, message.getEncoding());
                    message.getSerializer().writeMessage(message, writer, context);
                    writer.close();
                } catch (XMLStreamException e) {
                    LOG.error("CIC001400_Error_closing_output_stream", new Object[] {e.getMessage()}, e);
                    throw new XFireException(MESSAGES.getString("CIC001400_Error_closing_output_stream", e.getMessage()), e);
                }
                return;
            }
        } else {
            try {
                DeliveryChannel channel = ((JbiTransport) getTransport()).getContext().getDeliveryChannel();
                MessageExchangeFactory factory = channel.createExchangeFactory();
                if (context.getExchange().hasOutMessage()) {
                    InOut me = factory.createInOutExchange();
                    me.setInterfaceName((QName) context.getService().getProperty(JBI_INTERFACE_NAME));
                    me.setService((QName) context.getService().getProperty(JBI_SERVICE_NAME));
                    me.setEndpoint((ServiceEndpoint) context.getService().getProperty(JBI_ENDPOINT));
                    NormalizedMessage msg = me.createMessage();
                    me.setInMessage(msg);
                    msg.setContent(getContent(context, message));
                    if (!channel.sendSync(me)) {
                        throw new XFireException(MESSAGES.getString("CIC001401_Error_sending_jbi_exchange"));
                    }
                    if (me.getStatus() == ExchangeStatus.ERROR) {
                        me.setStatus(ExchangeStatus.DONE);
                        channel.send(me);
                        if (me.getError() != null) {
                            throw new XFireFault(me.getError(), XFireFault.RECEIVER);
                        } else if (me.getFault() != null){
                            // TODO: retrieve fault
                            throw new XFireFault(MESSAGES.getString("CIC001402_Fault_received"), XFireFault.RECEIVER);
                        } else {
                            throw new XFireFault(MESSAGES.getString("CIC001403_Unknown_error"), XFireFault.RECEIVER);
                        }
                    }
                    Source outSrc = me.getOutMessage().getContent();
                    me.setStatus(ExchangeStatus.DONE);
                    channel.send(me);

                    InMessage inMessage = new InMessage(sourceTransformer.toXMLStreamReader(outSrc), getUri());
                    getEndpoint().onReceive(context, inMessage);
                } else {
                    // TODO
                }
                
                
            } catch (Exception e) {
                LOG.error("CIC001404_Error_sending_exchange", 
                        e.getMessage(), e);
                throw new XFireException(MESSAGES.getString(
                        "CIC001404_Error_sending_exchange", e.getMessage()), e);
            }
        }
    }

    protected Source getContent(MessageContext context, OutMessage message) throws XMLStreamException, IOException, XFireException {
        ByteArrayOutputStream outStream = new ByteArrayOutputStream();
        XMLStreamWriter writer = outputFactory.createXMLStreamWriter(outStream, message.getEncoding());
        MessageSerializer serializer = context.getOutMessage().getSerializer();
        if (serializer == null)
        {
        	AbstractSoapBinding binding = (AbstractSoapBinding) context.getBinding();
            if (binding == null)
            {
                throw new XFireException(MESSAGES.getString("CIC001405_Binding_not_found"));
            }
            serializer = AbstractSoapBinding.getSerializer(binding.getStyle(), binding.getUse());
        }
        serializer.writeMessage(message, writer, context);
        writer.close();
        outStream.close();
        StreamSource src = new StreamSource(new ByteArrayInputStream(outStream.toByteArray()));
        return src;
    }
    
}
