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
 * @(#)EngineChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package driver;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.netbeans.enterprise.bpel.troubleticketwrapper.ScalabilityTestRequestType;
import org.netbeans.enterprise.bpel.troubleticketwrapper.TestOptionsType;

import com.sun.java.products.oss.xml.troubleticket.CreateTroubleTicketByValueRequest;

/**
 * @author mbhasin
 *
 */
class FirstMesssageSenderThread extends MessageSender {
    
    List inputMessages;

    
    /**
     * @param driver
     * @throws Exception 
     */
    public FirstMesssageSenderThread(Driver driver) throws Exception {
        this.driver = driver;
        seedMessages();
        seedTestOptionMessages();
    }
    
    private void seedMessages() throws Exception {
        ScalabilityTestRequestType inputMessage = null;
        this.inputMessages = new ArrayList();
        
        int count = driver.getThreads();
        
        for(int i =0; i < count; i++) {
            inputMessage = createInputMessage();
            inputMessages.add(inputMessage);
        }
    }
    
    /**
     * @return
     */
    private ScalabilityTestRequestType createInputMessage() {

        org.netbeans.enterprise.bpel.troubleticketwrapper.ScalabilityTestRequestType inputMessage = null;
        
        try {
            CreateTroubleTicketByValueRequest createByValue = Messages.getCreateTroubleTicketByValueRequest();

            inputMessage = new ScalabilityTestRequestType();
            inputMessage.setCreateTroubleTicketByValueRequest(createByValue);
        } catch (Exception ex) {
            System.out.println("Exception: " + ex.getMessage());
            ex.printStackTrace();
        }
        return inputMessage;
    }
    
    private ScalabilityTestRequestType getScalabilityTestRequestTypeMessage(int threadId) {
        return(ScalabilityTestRequestType) inputMessages.get(threadId);
    }
    
    /* (non-Javadoc)
     * @see driver.MessageSender#runTest()
     */
    public void runTest(int threadId) throws Exception {

        ScalabilityTestRequestType inputMessage = getScalabilityTestRequestTypeMessage(threadId);
        TestOptionsType testOptionsMessage = getTestOptionsTypeMessage(threadId);
        inputMessage.setTestOptions(testOptionsMessage);
        
        int iterations = driver.getIterations();
        
        for (int i = 0; i < iterations; i++) {
            
            setMessageId(testOptionsMessage);
            
            try {
                // Call Web Service Operation
                // send the first message
                org.netbeans.enterprise.bpel.troubleticketwrapper.CreateTroubleTicketPortType port = driver.getService().getCreateTTPort();
                // The message construction is done during class loading, to save processing
                port.createTTOperation(inputMessage);

            } catch (Throwable ex) {
                System.out.println("Exception: " + ex.getMessage());
                ex.printStackTrace();
            }
        }
    }
    
    public void run() {
        createExcutorServiceAndsubmitWorkers();
    }
}
