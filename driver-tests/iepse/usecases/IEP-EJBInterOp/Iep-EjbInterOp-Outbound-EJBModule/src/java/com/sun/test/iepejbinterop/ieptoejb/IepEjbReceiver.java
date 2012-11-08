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
 * @(#)IepEjbReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.test.iepejbinterop.ieptoejb;

import iep_ejbeventprocess_iep.IEPEJBStreamOutputMsgObj;
import javax.ejb.Stateless;
import javax.jws.WebService;

import javax.annotation.Resource;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;


/**
 *
 * @author vvenkataraman
 */
@Stateless
@WebService(serviceName = "OutputService_IEP_EJB_StreamOutput", portName = "OutputPort_IEP_EJB_StreamOutput", endpointInterface = "iep_ejbeventprocess_iep.OutputPtIEPEJBStreamOutput", targetNamespace = "Iep_EjbEventProcess_iep", wsdlLocation = "META-INF/wsdl/NewWebServiceFromWSDL/Iep-EjbEventProcess.wsdl")
public class IepEjbReceiver implements iep_ejbeventprocess_iep.OutputPtIEPEJBStreamOutput {
    
    @Resource(mappedName = "jms/FromIEPQueueFactory")
    private ConnectionFactory iepQueueFactory;

    @Resource(mappedName = "jms/FromIEPQueue")
    private Queue iepQueue;
    
    /** Creates a new instance of NewWebServiceFromWSDL */
    public IepEjbReceiver() {
    }

    public void iepEJBStreamOutput(IEPEJBStreamOutputMsgObj output) {
        this.sendToFromIEPQueue(output);
    }
    
    private void sendToFromIEPQueue(IEPEJBStreamOutputMsgObj output) {
        Connection connection = null;
        Session session = null;
        try { 
            connection = iepQueueFactory.createConnection();
            session = connection.createSession(false,Session.AUTO_ACKNOWLEDGE);
            MessageProducer messageProducer = session.createProducer(iepQueue);
            javax.jms.TextMessage tm = session.createTextMessage();
            
            String fromIEP = "\nBigInt = " + output.getBigIntField() 
                + "\nDouble = " + output.getDoubleField()
                + "\nInteger = " + output.getIntegerField()
                + "\nDate = " + output.getDateField().toString() 
                + "\nTime = " + output.getTimeField()
                + "\nTimeStamp = " + output.getTimeStampField()
                + "\nVarChar = " + output.getVarCharField();
            
            tm.setText(fromIEP);
            tm.setLongProperty("BigIntField", output.getBigIntField());
            tm.setDoubleProperty("DoubleField", output.getDoubleField());
            tm.setIntProperty("IntField", output.getIntegerField());
            tm.setStringProperty("VarCharField", output.getVarCharField());
            
            messageProducer.send(tm);
         } catch (Exception ex) {
            System.out.println("Unable to send message to Queue named FromIEPQueue (NewWebServiceFromWSDL) " + ex.getMessage());
            ex.printStackTrace();
        }finally {
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
    }
}
