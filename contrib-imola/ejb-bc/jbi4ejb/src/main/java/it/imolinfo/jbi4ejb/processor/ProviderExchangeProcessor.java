/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.processor;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.Jbi4EjbException;
import it.imolinfo.jbi4ejb.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbProviderEndpoint;
import it.imolinfo.jbi4ejb.jbi.xfire.EjbTransport;
import it.imolinfo.jbi4ejb.processor.transform.SourceTransformer;
import it.imolinfo.jbi4ejb.processor.transform.StringSource;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;

import javax.activation.DataHandler;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.attachments.Attachment;
import org.codehaus.xfire.attachments.Attachments;
import org.codehaus.xfire.attachments.JavaMailAttachments;
import org.codehaus.xfire.attachments.SimpleAttachment;
import org.codehaus.xfire.exchange.InMessage;
import org.codehaus.xfire.fault.XFireFault;
import org.codehaus.xfire.service.OperationInfo;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.transport.Channel;
import org.codehaus.xfire.transport.Transport;
import org.codehaus.xfire.util.jdom.StaxSerializer;


/**
 * The Class ProviderExchangeProcessor.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 * 
 */
public class ProviderExchangeProcessor implements ExchangeProcessor {

    /** The Logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(ProviderExchangeProcessor.class);   
    
    /** The endpoint. */ 
    private Jbi4EjbProviderEndpoint endpoint;
    
    /** The SourceTransformer. */
    private SourceTransformer transformer;
    
    /** message denormalizer. */
    private Jbi4EjbDenormalizer messageDenormalizer;
    
    /** message normalizer. */
    private Jbi4EjbNormalizer messageNormalizer;

    /**
     * Instantiates a new provider exchange processor.
     * 
     * @param endpoint the endpoint managed
     * 
     * @throws Jbi4EjbException if some problem occurs in creation
     */
    public ProviderExchangeProcessor(Jbi4EjbProviderEndpoint endpoint) throws Jbi4EjbException  {
        this.endpoint = endpoint;      
        transformer = new SourceTransformer();
        messageDenormalizer = new Jbi4EjbDenormalizer();
        messageNormalizer = new Jbi4EjbNormalizer();               
    }

    /**
     * Process the message exchange.
     * 
     * @param exchange
     *            the message exchange
     */
    public void process(MessageExchange exchange) {
        
        LOG.debug("Processing message exchange: " + exchange);

        // TODO implement      
        if (exchange.getStatus() == ExchangeStatus.DONE) {
            LOG.debug("ExchangeStatus.DONE");
            return;
        } else if (exchange.getStatus() == ExchangeStatus.ERROR) {
            LOG.debug("ExchangeStatus.ERROR");
            return;
        }
        
        try {
            XFire xfire = endpoint.getSuManager().getLifeCycle().getXfire();            
            Service service = endpoint.getXfireService();

            // Configures xfire
            // Transport t = xfire.getTransportManager().getTransport(EjbTransport.EJB_BINDING);            
            Transport t = xfire.getTransportManager().getTransport(EjbTransport.EJB_BINDING);
            
            ByteArrayOutputStream xfireOut = new ByteArrayOutputStream();
            Channel xfireChannel = t.createChannel();
            MessageContext xfireCtx = new MessageContext();
            xfireCtx.setXFire(xfire);
            xfireCtx.setService(service);
            xfireCtx.setProperty(Channel.BACKCHANNEL_URI, xfireOut);           
            xfireCtx.setExchange(new org.codehaus.xfire.exchange.MessageExchange(xfireCtx));
            InMessage xfireMsg = new InMessage();

            xfireCtx.getExchange().setInMessage(xfireMsg);
            if (exchange.getOperation() != null) {

                OperationInfo op = service.getServiceInfo().getOperation(
                        exchange.getOperation().getLocalPart());
                if (op != null) {
                    xfireCtx.getExchange().setOperation(op);
                } else {
                    LOG.debug("OperationInfo is null.");
                }
            }
            xfireCtx.setCurrentMessage(xfireMsg);
            
            // Gets the JBI message
            NormalizedMessage in = exchange.getMessage("in");           
            if (LOG.isDebugEnabled()) {
                String inMessage = "In message, before unwrapping: " + transformer.contentToString(in);
                LOG.debug(inMessage);
            }
            Jbi4EjbMessage inMsg = messageDenormalizer.denormalize(in, endpoint, exchange.getOperation());     
            if (LOG.isDebugEnabled()) {
                String inMessage = "In message, after unwrapping: " + transformer.toString(inMsg.getMessageSource());
                LOG.debug(inMessage);
            }
            
            // Sets the Source as XMLStreamReader to the xfireMessage            
            xfireMsg.setXMLStreamReader(transformer.toXMLStreamReader(inMsg.getMessageSource()));            

            if (in.getAttachmentNames() != null
                    && in.getAttachmentNames().size() > 0) {

                JavaMailAttachments attachments = new JavaMailAttachments();
                for (Iterator it = in.getAttachmentNames().iterator(); it.hasNext();) {
                    String name = (String) it.next();
                    DataHandler dh = in.getAttachment(name);
                    attachments.addPart(new SimpleAttachment(name, dh));
                }
                xfireMsg.setAttachments(attachments);
            }

            xfireChannel.receive(xfireCtx, xfireMsg);
            xfireChannel.close();

            // Set response or DONE status
            if (isInAndOut(exchange)) {
                if (xfireCtx.getExchange().hasFaultMessage()
                        && xfireCtx.getExchange().getFaultMessage().getBody() != null) {

                    
                    LOG.debug("Fault message");
                    Fault fault = exchange.createFault();

                    // The XFireFault is a SOAP Fault wrapper
                    XFireFault xFault = (XFireFault) xfireCtx.getExchange().getFaultMessage().getBody();                   
                    
                    // If the XFireFault has details, the Fault is declared.
                    if (xFault.hasDetails()) {
                        // Serialize correctly the fault to a SOAP message
                        Source faultSource = createJBIFaultSourceFromSOAPFault(xfireCtx);
                        
                        // Gets the fault message name from the xFire fault detail
                        String faultName = ((org.jdom.Element)xFault.getDetail().getContent().get(0)).getName();
                        
                        // Normalize the fault message
                        messageNormalizer.normalizeFault(faultSource, fault, endpoint, exchange.getOperation(),  faultName, inMsg.isWrapped());                                               

                        exchange.setFault(fault);
                    } else {
                        // Sets the cause. Notice that (from the specs):
                        // Used to specify the source of a failure status.
                        // Invoking this method automatically adjusts the status of the
                        // ME to ExchangeStatus.ERROR.
                        // So, the cause can be setted only if no Fault is setted.

                        if (xFault.getCause() instanceof Exception) {
                            exchange.setError((Exception) xFault.getCause());
                        } else {
                            exchange.setError(new Exception(xFault.getCause()));
                        }
                    }

                } else {
                    NormalizedMessage outMsg = exchange.createMessage();
                    Attachments attachments = xfireCtx.getCurrentMessage().getAttachments();
                    if (attachments != null) {
                        for (Iterator it = attachments.getParts(); it.hasNext();) {
                            Attachment att = (Attachment) it.next();
                            outMsg.addAttachment(att.getId(), att.getDataHandler());
                        }
                    }                    
                    
                    // Gets the output source                    
                    InputStream inputStream = new ByteArrayInputStream(xfireOut.toByteArray());
                    Source outSource = transformer.toDOMSourceFromStream(new StreamSource(inputStream));
                    
                    if (LOG.isDebugEnabled()) {
                        String inMessage = "Out message, before wrapping: " + transformer.toString(outSource);
                        LOG.debug(inMessage);
                    }                    
                    // Normalize the source and sets to the output message.
                    messageNormalizer.normalize(outSource, outMsg, endpoint, exchange.getOperation(), inMsg.isWrapped());
                    
                    if (LOG.isDebugEnabled()) {
                        String inMessage = "Out message, after wrapping: " + transformer.contentToString(outMsg);
                        LOG.debug(inMessage);
                    }
                    
                    exchange.setMessage(outMsg, "out");
                }
            } else {
            	LOG.warn("EJB000606_Error_in_exchange_type", new Object[]{exchange.getClass().getName()});
            }

            LOG.debug("before - Channel.send");
            RuntimeHelper.getDeliveryChannel().send(exchange);
            LOG.debug("after - Channel.send");

        } catch (Exception ex)  { 
            LOG.error("EJB000607_Error_in_message_exchange", new Object[]{ex.getMessage()});
            // No exception is thrown...
            exchange.setError(ex);
        }
    }
   
    /**
     * Checks if the message is InOut.
     * 
     * @param exchange the message exchange
     * 
     * @return true, if is in and out
     */
    protected boolean isInAndOut(MessageExchange exchange) {
        return exchange instanceof InOut || exchange instanceof InOptionalOut;
    }

    /**
     * Create a JBI Fault <code>Source</code> using the XFireFault details
     * (that are JDOM elements!!!).
     * 
     * @param ctx the message context
     * 
     * @return the JBI fault <code>Source</code>
     * 
     * @throws XMLStreamException if some problem occurs in reading the XMLStream
     */
    private Source createJBIFaultSourceFromSOAPFault(MessageContext ctx) throws XMLStreamException {

        LOG.debug("Creating JBI fault from xfire fault");
        StringWriter strWriter = new StringWriter();
        XMLOutputFactory witerFactory = XMLOutputFactory.newInstance();
        XMLStreamWriter writer = witerFactory.createXMLStreamWriter(strWriter);
        XFireFault fault = (XFireFault) ctx.getExchange().getFaultMessage().getBody();
        LOG.debug("Xfire fault: " + fault);
        if (fault.hasDetails())
        {
            // It's a JDom Element :-(
            org.jdom.Element detail = fault.getDetail();
            StaxSerializer serializer = new StaxSerializer();
            List details = detail.getContent();
            for (int i = 0; i < details.size(); i++)
            {
                serializer.writeElement((org.jdom.Element) details.get(i), writer);
            }
        }
        writer.flush();
        
        StringSource stringSource = new StringSource(strWriter.toString());

        LOG.debug("Fault message produced: " + stringSource);

        return stringSource;
    }
    
    

}


