/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.jbi.processor.wsdl11wrapper.WrapperUtil;
import it.imolinfo.jbi4cics.jbi.xfire.JbiTransport;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;

import javax.activation.DataHandler;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;

import org.apache.servicemix.common.ExchangeProcessor;
import org.apache.servicemix.jbi.jaxp.SourceTransformer;
import org.apache.servicemix.jbi.jaxp.StAXSourceTransformer;
import org.apache.servicemix.jbi.jaxp.StringSource;
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
import org.jdom.Element;

public class Jbi4cicsExchangeProcessor implements ExchangeProcessor {
  
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(Jbi4cicsExchangeProcessor.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(Jbi4cicsExchangeProcessor.class);

  public static final String SOAP_FAULT_CODE = "org.apache.servicemix.soap.fault.code"; 
  public static final String SOAP_FAULT_SUBCODE = "org.apache.servicemix.soap.fault.subcode";
  public static final String SOAP_FAULT_REASON = "org.apache.servicemix.soap.fault.reason";
  public static final String SOAP_FAULT_NODE = "org.apache.servicemix.soap.fault.node";
  public static final String SOAP_FAULT_ROLE = "org.apache.servicemix.soap.fault.role";
  
  protected DeliveryChannel channel;
  protected Jbi4cicsEndpoint endpoint;
  protected StAXSourceTransformer transformer;
  
  public Jbi4cicsExchangeProcessor(Jbi4cicsEndpoint endpoint) {
      this.endpoint = endpoint;
      this.transformer = new StAXSourceTransformer();
  }

  public void process(MessageExchange exchange) throws Exception {
      
      // true, if the recieved message is a wsdl-11-wrappedm message
      boolean messageWrapped = false;
      
      if (exchange.getStatus() == ExchangeStatus.DONE) {
          return;
      } else if (exchange.getStatus() == ExchangeStatus.ERROR) {
          return;
      }

      // TODO: clean this code
      XFire xfire = endpoint.getXFire();
      Service service = endpoint.getXFireService();
      Transport t = xfire.getTransportManager().getTransport(JbiTransport.JBI_BINDING);
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      Channel c = t.createChannel();
      MessageContext ctx = new MessageContext();
      ctx.setXFire(xfire);
      ctx.setService(service);
      ctx.setProperty(Channel.BACKCHANNEL_URI, out);
      ctx.setExchange(new org.codehaus.xfire.exchange.MessageExchange(ctx));
      InMessage msg = new InMessage();
      ctx.getExchange().setInMessage(msg);
      if (exchange.getOperation() != null) {
          OperationInfo op = service.getServiceInfo().getOperation(exchange.getOperation().getLocalPart());
          if (op != null) {
              ctx.getExchange().setOperation(op);
          }
      }
      ctx.setCurrentMessage(msg);
      NormalizedMessage in = exchange.getMessage("in");
      
      Source inStr = new StringSource(in.toString());
    
      LOG.debug("in.getContent:" + in.getContent().getClass().getName());
      LOG.debug("inStr:" + inStr); 
      
      
      SourceTransformer sourceTransformer = new SourceTransformer();
      String receivedMessage = sourceTransformer.contentToString(in);
      
      LOG.debug("Received InMessage: " + receivedMessage);
      
      
      // Tests if the message is a wsdl-11-wrapped
      // Document inMessageDoc = getXMLDom(in.getContent());
      XMLStreamReader xmlStreamReader = getXMLStreamReader(in.getContent());
      
      messageWrapped = WrapperUtil.isMessageWrapped(xmlStreamReader);
      
      LOG.debug("IsMessage Wrapped: " + messageWrapped);      
      if (messageWrapped) {
          LOG.debug("Message to be unwrapped");
          // If its a wsdl-11-wrapped message, unwraps-it
          msg.setXMLStreamReader(WrapperUtil.unWrapMessage(xmlStreamReader));
      } else {
          LOG.debug("Before setting stream reader");          
          msg.setXMLStreamReader(getXMLStreamReader(new StringSource(receivedMessage)));
      }     
      
      if (in.getAttachmentNames() != null && in.getAttachmentNames().size() > 0) {
          JavaMailAttachments attachments = new JavaMailAttachments();
          for (Iterator it = in.getAttachmentNames().iterator(); it.hasNext();) {
              String name = (String) it.next();
              DataHandler dh = in.getAttachment(name);
              attachments.addPart(new SimpleAttachment(name, dh));
          }
          msg.setAttachments(attachments);
      }
      c.receive(ctx, msg);
      c.close();
      
      // Set response or DONE status
      if (isInAndOut(exchange)) {
          if (ctx.getExchange().hasFaultMessage() && ctx.getExchange().getFaultMessage().getBody() != null) {              
              
              Fault fault = exchange.createFault();                           

              // The XFireFault is a SOAP Fault wrapper
              XFireFault xFault = (XFireFault) ctx.getExchange().getFaultMessage().getBody();                    

              // If the XFireFault has details, the Fault is declared.              
              if (xFault.hasDetails()) {
                  // Serialize correctly the fault to a SOAP message
                  Source faultSource = createJBIFaultSourceFromSOAPFault(ctx);                                                                             
                  fault.setContent(faultSource);       
                  
                  exchange.setFault(fault);                  
              } else {   
                  // Sets the cause. Notice that (from the specs):
                  // Used to specify the source of a failure status. Invoking this method automatically adjusts the status of the
                  // ME to ExchangeStatus.ERROR.
                  // So, the cause can be setted only if no Fault is setted.

                  if (xFault.getCause() instanceof Exception) {
                      exchange.setError((Exception) xFault.getCause());
                  } else {
                      exchange.setError(new Exception(xFault.getCause()));
                  }
              }
                                                                                                                                                                                                                                                  
          } else {
              String charSet = ctx.getOutMessage().getEncoding();
              
              NormalizedMessage outMsg = exchange.createMessage();
              Attachments attachments = ctx.getCurrentMessage().getAttachments();
              if (attachments != null) {
                  for (Iterator it = attachments.getParts(); it.hasNext();) {
                      Attachment att = (Attachment) it.next();
                      outMsg.addAttachment(att.getId(), att.getDataHandler());
                  }
              }
              
              LOG.debug("Output message(before wrapping): " + out);
              
              OperationInfo op = service.getServiceInfo().getOperation(exchange.getOperation().getLocalPart());
              QName messageQName = op.getOutputMessage().getName();
              
              // Ok for the operation Name.
              // If the in-message is wrapped, also the response should be wrapped.
              Source msgSource = null;
              if (messageWrapped) {
                  msgSource = WrapperUtil.jbiMessageWrapper(out.toString(charSet), messageQName, op.getName());
              } else {
                  msgSource = new StringSource(out.toString(charSet));
              }                                                           
              
              outMsg.setContent(msgSource);
              
              LOG.debug("Sending outMessage: " + sourceTransformer.contentToString(outMsg));  
              
              exchange.setMessage(outMsg, "out");
          }
      } else {
          exchange.setStatus(ExchangeStatus.DONE);
      }
      channel.send(exchange);
  }

  public void start() throws Exception {
      channel = endpoint.getServiceUnit().getComponent().getComponentContext().getDeliveryChannel();
  }

  public void stop() throws Exception {
  }

  protected XMLStreamReader getXMLStreamReader(Source source) throws TransformerException, XMLStreamException {
      return transformer.toXMLStreamReader(source);
  }
  
  protected boolean isInAndOut(MessageExchange exchange) {
      return exchange instanceof InOut || exchange instanceof InOptionalOut;
  }
   
  
  /**
   * Create a JBI Fault <code>Source</code> using the XFireFault details. 
   * @return Source    The source
   * @param  ctx    The message context
   * @throws XMLStreamException    The XML Stream Exception
   * @throws XFireFault    The XFire Foult Exception 
   */
  private Source createJBIFaultSourceFromSOAPFault(MessageContext ctx) throws XMLStreamException, XFireFault {

      StringWriter strWriter = new StringWriter();
      XMLOutputFactory witerFactory = XMLOutputFactory.newInstance();                           
      XMLStreamWriter writer = witerFactory.createXMLStreamWriter(strWriter);
      XFireFault fault = (XFireFault) ctx.getExchange().getFaultMessage().getBody();
      
      if (fault.hasDetails())
      {
          Element detail = fault.getDetail();                   
          StaxSerializer serializer = new StaxSerializer();
          List details = detail.getContent();
          for (int i = 0; i < details.size(); i++)
          {
              serializer.writeElement((Element) details.get(i), writer);
          }

      }                       
      writer.flush();                  
      StringSource stringSource = new StringSource(strWriter.toString());
       
      LOG.debug("Fault message produced: " + stringSource);
      
      return stringSource;
  }
  

}
