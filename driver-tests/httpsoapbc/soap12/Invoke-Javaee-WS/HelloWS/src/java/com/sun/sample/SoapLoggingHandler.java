/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.sample;

import java.util.Iterator;
import java.util.Set;
import javax.xml.namespace.QName;
import javax.xml.soap.MimeHeader;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.handler.soap.SOAPHandler;
import javax.xml.ws.handler.soap.SOAPMessageContext;

/**
 *
 * @author sbiswas
 */
public class SoapLoggingHandler implements SOAPHandler<SOAPMessageContext> {

    

    public boolean handleMessage(SOAPMessageContext context) {
        logToSystemOut(context);
        return true;

    }

    public boolean handleFault(SOAPMessageContext context) {
        logToSystemOut(context);
        return true;
    }

    public Set<QName> getHeaders() {
       return null;

    }

    public void close(MessageContext context) {
        
    }

   private void logToSystemOut(SOAPMessageContext smc) {
        Boolean outboundProperty = (Boolean)
            smc.get (MessageContext.MESSAGE_OUTBOUND_PROPERTY);
        
        if (outboundProperty.booleanValue()) {
            System.out.println("\nOutbound message:");
            printMimeHeader(smc);
        } else {
            System.out.println("\nInbound message:");
            printMimeHeader(smc);
            
            
        }
        
        SOAPMessage message = smc.getMessage();
        try {
            message.writeTo(System.out);
            System.out.println("");   
        } catch (Exception e) {
            System.out.println("Exception in handler: " + e);
        }
    }

    private void printMimeHeader(SOAPMessageContext smc) {


        MimeHeaders hr = smc.getMessage().getMimeHeaders();
        Iterator iter = hr.getAllHeaders();
        while (iter.hasNext()) {

            MimeHeader h = (MimeHeader) iter.next();
            System.out.println(h.getName() + " = " + h.getValue());
        }
    }


}
