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

import java.io.OutputStream;
import java.util.List;

import javax.xml.bind.Marshaller;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.apache.cxf.interceptor.AbstractOutDatabindingInterceptor;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.jaxb.io.DataWriterImpl;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageContentsList;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.BindingMessageInfo;
import org.apache.cxf.service.model.BindingOperationInfo;
import org.apache.cxf.service.model.MessagePartInfo;

/**
 * Jbi4Corba class to manage array marshalling errors (based on BareOutInterceptor CXF class).
 * @author mpiraccini@imolinfo.it
 */
@SuppressWarnings("unchecked")
public class Jbi4CorbaBareOutInterceptor extends
		AbstractOutDatabindingInterceptor {
		
    /** The logger. */
    private static final Logger LOG = 
    	LoggerFactory.getLogger(Jbi4CorbaBareOutInterceptor.class);	

    /**
     * Constructor.
     */
	public Jbi4CorbaBareOutInterceptor() {
		super(Phase.MARSHAL);
		addAfter(Jbi4CorbaWrappedOutInterceptor.class.getName());
	}
	
	/**
	 * Constructor.	
	 * @param phase
	 */
	public Jbi4CorbaBareOutInterceptor(String phase) {
        super(phase);
        addAfter(Jbi4CorbaWrappedOutInterceptor.class.getName());
    }
		
	public void handleMessage(Message message) {
		
		LOG.debug("handling message");
		Exchange exchange = message.getExchange();
		BindingOperationInfo operation = (BindingOperationInfo) exchange
				.get(BindingOperationInfo.class.getName());

		if (operation == null) {
			return;
		}

		MessageContentsList objs = MessageContentsList.getContentsList(message);
		if (objs == null || objs.size() == 0) {
			return;
		}

		List<MessagePartInfo> parts = null;
		BindingMessageInfo bmsg = null;
		boolean client = isRequestor(message);

		if (!client) {
			if (operation.getOutput() != null) {
				bmsg = operation.getOutput();
				parts = bmsg.getMessageParts();
			} else {
				// partial response to oneway
				return;
			}
		} else {
			// Input (for the consumer...)
			bmsg = operation.getInput();
			parts = bmsg.getMessageParts();
		}

		Service service = exchange.get(Service.class);
		XMLStreamWriter origXmlWriter = message.getContent(XMLStreamWriter.class);        
        XMLStreamWriter xmlWriter = origXmlWriter;
        // If there's somthing before (the wrapper, if present) adds it.
        if (xmlWriter != null) {
            try {
                xmlWriter.writeCharacters("");
                xmlWriter.flush();
            } catch (XMLStreamException e) {
                throw new Fault(e);
            }
        }
        
        OutputStream out = message.getContent(OutputStream.class);
        if (out != null) {
        
        	DataWriterImpl writerImpl = (DataWriterImpl) getDataWriter(message, service, OutputStream.class);
			        	
        	for (MessagePartInfo part : parts) {
        		if (objs.hasValue(part)) {
        			Object o = objs.get(part);
        			Marshaller marshaller = writerImpl.createMarshaller(o, part);
        			OutMarshallerHelper.marshal(marshaller, o, part, out);
        		}
        	}
        } else {
        	DataWriterImpl dataWriter = (DataWriterImpl) getDataWriter(message, service, XMLStreamWriter.class);            
            for (MessagePartInfo part : parts) {
                if (objs.hasValue(part)) {
                    Object o = objs.get(part);
                    Marshaller marshaller = dataWriter.createMarshaller(o, part);
                    OutMarshallerHelper.marshal(marshaller, o, part, xmlWriter);                    
                }
            }
        }	
	}
	
}
