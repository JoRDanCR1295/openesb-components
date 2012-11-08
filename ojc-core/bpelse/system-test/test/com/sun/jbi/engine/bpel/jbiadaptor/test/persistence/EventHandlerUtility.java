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
 * @(#)CorrelationUtility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.rmi.server.UID;
import java.util.Properties;

import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;

/**
 * Persistence utility class for correlation unit tests.
 * 
 * @author Kevan Simpson
 */
public class EventHandlerUtility extends UtilityClass {
    

    public void associateEventHandlerInvokeChannel(final Properties props, 
            final Engine eng, final DeploymentBindings deplBindings) 
    throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);

        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            String mOneWayInvokeMEId;
            Transaction mTx;
            MessageContainer mContainer;
            
            String mOneWayInvokeMEIdNested;
            Transaction mTxNested;
            MessageContainer mContainerNested;
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, 
            		QName operation, boolean oneWay, RBPELProcess process) {
                if (oneWay) {
                    if (partnerLink.getStaticModel().getName().equals("PartnerLink3")) {
                        mTx = msgContainer.getTransaction();

                        String service = props.getProperty("ONEVENT_SERVICE_NAME");
                        QName serviceQName = QName.valueOf(service);
                        String endPoint = props.getProperty("ONEVENT_ENDPOINT");

                        JBIMessageImpl jbiMsg = (JBIMessageImpl) msgContainer.getContent();
                        InComingKey key = deplBindings.createInComingBindingsKey(
                                serviceQName, endPoint, operation.getLocalPart());
                        final InComingEventModel model = deplBindings.getInComingEventModel(key);

                        mOneWayInvokeMEId = new UID().toString();
                        mContainer = 
                            MessageContainerFactory.createMessage(mOneWayInvokeMEId, jbiMsg, null, mTx);
                        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
                        System.out.println("sending one way invoke to engine to instantiate event handler");
                        eng.process(event, mContainer);
                        return mOneWayInvokeMEId;
                    } else if (partnerLink.getStaticModel().getName().equals("PartnerLink4")) {
                        // completing the assignment inside the event handler onEvent's scope
                        /************VERY IMPOPRTANT ************/
                        eng.process();
                        String waitTime = props.getProperty("WAIT_TIME");
                        if (!Utility.isEmpty(waitTime)) {
                            long timeOut = 0;
                            try {
                                timeOut = Long.parseLong(waitTime) * 1000;
                                System.out.println("UtilityClass.doWait(): "+ timeOut);
                                Thread.sleep(timeOut);
                            } catch (Exception e) {
                                e.printStackTrace();
                                throw new RuntimeException(e);
                            }
                        }
                        /************VERY IMPOPRTANT ************/

                        String doneMEId = new UID().toString();
                        // send the second invoke's done status
                        MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(doneMEId, null, null);
                        // This is used only for one way invokes
                        statusContainer.setTransaction(msgContainer.getTransaction());
                        ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                                model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
                        eng.process(event, statusContainer);
                        return doneMEId;
                    } else if (partnerLink.getStaticModel().getName().equals("PartnerLink6")) {
                        mTxNested = msgContainer.getTransaction();

                        String service = props.getProperty("NESTED_ONEVENT_SERVICE_NAME");
                        QName serviceQName = QName.valueOf(service);
                        String endPoint = props.getProperty("NESTED_ONEVENT_ENDPOINT");

                        JBIMessageImpl jbiMsg = (JBIMessageImpl) msgContainer.getContent();
                        InComingKey key = deplBindings.createInComingBindingsKey(
                                serviceQName, endPoint, operation.getLocalPart());
                        final InComingEventModel model = deplBindings.getInComingEventModel(key);

                        mOneWayInvokeMEIdNested = new UID().toString();
                        mContainerNested = 
                            MessageContainerFactory.createMessage(mOneWayInvokeMEIdNested, jbiMsg, null, mTxNested);
                        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
                        System.out.println("sending one way invoke to engine to instantiate nested event handler");
                        eng.process(event, mContainerNested);
                        return mOneWayInvokeMEIdNested;
                    }
                }
                return null;
            }

            /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#reply(com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
             */
            public void reply(MessageContainer msgContainer) {
                UtilityClass.acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
            }

            /** @see com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor#sendInOnlyRequestDoneStatus(java.lang.String)
             */
            public void sendInOnlyRequestDoneStatus(String msgExchangeId) {
                //super.sendInOnlyRequestDoneStatus(msgExchangeId);
                if (mContainerNested == null) {
                    MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(mOneWayInvokeMEId, null, null);
                    //sending DONE for the first invoke
                    statusContainer.setTransaction(mTx);
                    ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                            model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
                    eng.process(event, statusContainer);
                    
                } else {
                    MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(mOneWayInvokeMEIdNested, null, null);
                    //sending DONE for the nested invoke
                    statusContainer.setTransaction(mTxNested);
                    ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                            model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
                    eng.process(event, statusContainer);
                    
                }
            }
        };
        eng.setOutChannel(channel);        

    }

    public void initiateBPInstance_FOR(final Properties props, 
            final Engine engine, DeploymentBindings deplBindings) throws Exception {
        
        super.initiateBPInstance(props, engine, deplBindings);
        String testMode = System.getProperty("TEST.MODE");
        if (!testMode.equals(EngineDriver.TestMode.RECOVER.toString())) {
            doWait(props, "FIRST_WAIT_TIME");
            engine.process();
        }
    }

    public void initiateBPInstance_RepeatEvery(final Properties props, 
            final Engine engine, DeploymentBindings deplBindings) throws Exception {
        
        super.initiateBPInstance(props, engine, deplBindings);
        
        doWait(props, "FIRST_WAIT_TIME");
        engine.process();
        doWait(props, "SECOND_WAIT_TIME");
        engine.process();
        doWait(props, "THIRD_WAIT_TIME");
        engine.process();
    }
    
    static void doWait(Properties props, String waitProp) throws Exception {
        long timeOut = 0;
        try {
            timeOut = Long.parseLong(props.getProperty(waitProp)) * 1000;
        }
        catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
        System.out.println("UtilityClass.doWait(): "+ timeOut);
        Thread.sleep(timeOut);
    }
    
    public void recover_FOR(Properties props, Engine engine, DeploymentBindings depBindings) throws Exception {
        super.recover(props, engine, depBindings);

        doWait(props, "FIRST_WAIT_TIME_AFTER_RECOVERY");
        engine.process();
        //doWait(props, "FIRST_WAIT_TIME");
        //engine.process();
    }
    
    public void recover_RepeatEvery(Properties props, Engine engine, DeploymentBindings depBindings) throws Exception {
        super.recover(props, engine, depBindings);
        engine.process();
        doWait(props, "FIRST_WAIT_TIME");
        engine.process();
        doWait(props, "SECOND_WAIT_TIME");
        engine.process();
        doWait(props, "THIRD_WAIT_TIME");
        engine.process();
    }
    

    public void associateEHChannel_forvariablesInEH(final Properties props, 
            final Engine eng, final DeploymentBindings deplBindings) 
    throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);

        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            String mOneWayInvokeMEId;
            Transaction mTx;
            MessageContainer mContainer;

            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, 
            		QName operation, boolean oneWay, RBPELProcess process) {
                if (oneWay) {
                    if (partnerLink.getStaticModel().getName().equals("PartnerLink3")) {
                        mTx = msgContainer.getTransaction();

                        String service = props.getProperty("ONEVENT_SERVICE_NAME");
                        QName serviceQName = QName.valueOf(service);
                        String endPoint = props.getProperty("ONEVENT_ENDPOINT");

                        JBIMessageImpl jbiMsg = (JBIMessageImpl) msgContainer.getContent();
                        InComingKey key = deplBindings.createInComingBindingsKey(
                                serviceQName, endPoint, operation.getLocalPart());
                        final InComingEventModel model = deplBindings.getInComingEventModel(key);

                        mOneWayInvokeMEId = new UID().toString();
                        mContainer = 
                            MessageContainerFactory.createMessage(mOneWayInvokeMEId, jbiMsg, null, mTx);
                        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
                        System.out.println("sending one way invoke to engine to instantiate event handler");
                        eng.process(event, mContainer);
                        return mOneWayInvokeMEId;
                    } else if (partnerLink.getStaticModel().getName().equals("PartnerLink4")) {

                        String doneMEId = new UID().toString();
                        // send the second invoke's done status
                        MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(doneMEId, null, null);
                        // This is used only for one way invokes
                        statusContainer.setTransaction(msgContainer.getTransaction());
                        ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                                model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
                        eng.process(event, statusContainer);
                        return doneMEId;
                    }
                }
                return null;
            }

            /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#reply(com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
             */
            public void reply(MessageContainer msgContainer) {
                UtilityClass.acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
            }

            /** @see com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor#sendInOnlyRequestDoneStatus(java.lang.String)
             */
            public void sendInOnlyRequestDoneStatus(String msgExchangeId) {
                MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(mOneWayInvokeMEId, null, null);
                //sending DONE for the first invoke
                statusContainer.setTransaction(mTx);
                ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                        model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
                eng.process(event, statusContainer);
            }
        };
        eng.setOutChannel(channel);        

    }

    public void associate_EH_FOR_Channel(final Properties props, 
            final Engine eng, final DeploymentBindings deplBindings) 
    throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);

        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, 
            		QName operation, boolean oneWay, RBPELProcess process) {
                if (oneWay) {
                    String testMode = System.getProperty("TEST.MODE");
                    int crashPoint = Integer.valueOf(System.getProperty("PERSIST.CRASHPOINT")).intValue();
                    System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Crash point >>>>>>> " + crashPoint);
                    if (!testMode.equals(EngineDriver.TestMode.RECOVER.toString())
                            || (testMode.equals(EngineDriver.TestMode.RECOVER.toString()) 
                                    && (crashPoint == 1 || crashPoint == 2))) {
                        try {
                            doWait(props, "FIRST_WAIT_TIME");
                        } catch (Exception ex) {
                            throw new RuntimeException(ex);
                        }
                        eng.process();
                    }
                    
                    
                    String messageExchangeId = new UID().toString();
                    MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(messageExchangeId, null, null);
                    //This is used only for one way invokes.
                    statusContainer.setTransaction(msgContainer.getTransaction());
                    ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                            model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
                    eng.process(event, statusContainer);
                    return messageExchangeId;
                }
                return null;
            }

            /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#reply(com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
             */
            public void reply(MessageContainer msgContainer) {
                UtilityClass.acceptMessageForTest(props, msgContainer, model, eng, msgContainer.getId());
            }

        };
        eng.setOutChannel(channel);        

    }
}
