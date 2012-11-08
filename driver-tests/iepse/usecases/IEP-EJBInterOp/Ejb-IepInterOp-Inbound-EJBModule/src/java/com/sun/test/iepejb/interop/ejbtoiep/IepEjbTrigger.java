/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)IepEjbTrigger.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.test.iepejb.interop.ejbtoiep;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.GregorianCalendar;
import javax.ejb.Stateless;
import javax.jms.Session;
import javax.jws.WebService;
import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.ws.WebServiceRef;

import javax.annotation.Resource;
import javax.jms.QueueConnection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageConsumer;
import javax.jms.Queue;
import javax.jms.QueueSession;
import javax.jms.TextMessage;
import javax.jms.QueueReceiver;

/**
 *
 * @author vvenkataraman
 */
@Stateless
@WebService(serviceName = "iepTriggerWSDLService", portName = "iepTriggerWSDLPort", endpointInterface = "org.netbeans.j2ee.wsdl.ieptriggerwsdl.IepTriggerWSDLPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/iepTriggerWSDL", wsdlLocation = "META-INF/wsdl/NewWebServiceFromWSDL/iepTriggerWSDL.wsdl")
public class IepEjbTrigger implements org.netbeans.j2ee.wsdl.ieptriggerwsdl.IepTriggerWSDLPortType {
    
    private static String SUCCESS = "SUCCESS";
    
    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/Iep-EjbEventProcess/Iep-EjbEventProcess.wsdl")
    private com.sun.test.iepejb.interop.ejbtoiep.InputService service;
    
    @Resource(mappedName = "jms/FromIEPQueueFactory")
    private ConnectionFactory iepQueueFactory;
    
    @Resource(mappedName = "jms/FromIEPQueue")
    private Queue iepQueue;
    
    private String mMessageStr = "";
    private long mBigIntField = 1234567890;
    private double mDoubleField = 1234567890.99;
    private int mIntField = 12345;
    private String mVarCharField = "Information Received from EJB Web Service";
    /**
     * Creates a new instance of IepEjbTrigger
     */
    public IepEjbTrigger() {
    }
    
    public org.netbeans.xml.schema.newxmlschema.TriggerOutput iepTriggerWSDLOperation(org.netbeans.xml.schema.newxmlschema.TriggerInput inputPart) {
        String retMsg = "";
        org.netbeans.xml.schema.newxmlschema.TriggerOutput output = new org.netbeans.xml.schema.newxmlschema.TriggerOutput();
        retMsg = this.callIEP();
        if (retMsg.equals(SUCCESS)) {
            retMsg = this.receiveFromIEPQueue();
        }
        output.setOutputMsg(retMsg);
        return output;
    }
    
    private String callIEP() {
        String retMsg = "";
        try {
            com.sun.test.iepejb.interop.ejbtoiep.InputPt port = service.getInputPort();
            com.sun.test.iepejb.interop.ejbtoiep.IEPEJBSteamInputMsgObj input = new com.sun.test.iepejb.interop.ejbtoiep.IEPEJBSteamInputMsgObj();
            
            input.setBigIntField(mBigIntField);
            input.setDoubleField(mDoubleField);
            input.setIntegerField(mIntField);
            input.setVarCharField(mVarCharField);
            
            java.util.GregorianCalendar tempCal = new java.util.GregorianCalendar();
            javax.xml.datatype.XMLGregorianCalendar calender = (javax.xml.datatype.DatatypeFactory.newInstance()).newXMLGregorianCalendar(tempCal);
            
            input.setDateField(calender);
            input.setTimeField(calender);
            input.setTimeStampField(calender);
            
            this.mMessageStr = "\nBigInt = " 
                    + input.getBigIntField()
                    + "\nDouble = " + input.getDoubleField()
                    + "\nInteger = " + input.getIntegerField()
                    + "\nDate = " + input.getDateField().toString()
                    + "\nTime = " + input.getTimeField()
                    + "\nTimeStamp = " + input.getTimeStampField()
                    + "\nVarChar = " + input.getVarCharField();
            
            port.iepEJBSteamInput(input);
            retMsg = SUCCESS;
            
        } catch (Exception ex) {
            System.out.println("**************************** Unable to call IEP (IepEjbTrigger) " + ex.getMessage());
            ex.printStackTrace();
            retMsg = "FAILED\n" +
                    "Exception Message: " + ex.getMessage() +
                    "\n" + ex.toString();
            System.out.println("**************************** Unable to call IEP (IepEjbTrigger) ");
        }
        return retMsg;
    }
    
    private String receiveFromIEPQueue() {
        String retMsg = SUCCESS;
        QueueConnection connection = null;
        QueueSession session = null;
        Message message = null;
        TextMessage textMessage = null;
        
        try {
            connection = (QueueConnection) iepQueueFactory.createConnection();
            connection.start();
            session = connection.createQueueSession(false,Session.AUTO_ACKNOWLEDGE);
            QueueReceiver queueReceiver = session.createReceiver(iepQueue);
            
            for (int ii = 0; ii < 10; ii++) {
                message = queueReceiver.receive(10000);
                if (message != null) {
                    break;
                }
            }
            if (message != null) {
                textMessage = (TextMessage) message;
                
                long tmpBigIntField = textMessage.getLongProperty("BigIntField");
                double tmpDoubleField = textMessage.getDoubleProperty("DoubleField");
                int tmpIntField = textMessage.getIntProperty("IntField");
                String tmpVarCharField = textMessage.getStringProperty("VarCharField");
                
                String receivedMsg = textMessage.getText();
                
                if (this.mMessageStr.equals(receivedMsg)) {
                    retMsg = SUCCESS;
                } else {
                    System.out.println("*********************** SEND *******************");
                    System.out.println(this.mMessageStr);
                    System.out.println("*********************** RECEIVED ***************");
                    System.out.println(receivedMsg);
                    System.out.println("************************************************");
                    if (this.mBigIntField == tmpBigIntField &&
                            this.mDoubleField == tmpDoubleField &&
                            this.mIntField == tmpIntField &&
                            this.mVarCharField.equals(tmpVarCharField)) {
                        retMsg = SUCCESS;
                    } else {
                        retMsg = "\nSEND = " + this.mMessageStr
                                + "\nRECEIVED = " + receivedMsg;
                    }
                }
            } else {
                retMsg = "Message back from IEP is not received tried 10 times with 10000 milliseconds interval";
                System.out.println("********* (IepEjbTrigger) retMsg = " + retMsg);
            }
            
        } catch (Exception ex) {
            System.out.println("Unable to receive message from Queue named FromIEPQueue (IepEjbTrigger)");
            ex.printStackTrace();
            retMsg = "Unable to receive message from Queue Named FromIEPQueue " + ex.getMessage();
        } finally {
            if (session != null) {
                try {
                    session.close();
                } catch (Exception ex1) {
                }
            }
            if (connection != null) {
                try {
                    connection.close();
                } catch (Exception ex1) {
                }
            }
        }
        return retMsg;
    }
}
