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
 * @(#)ConsumerImpl.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.res.impl;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.security.auth.Subject;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.api.MessageException;
import org.glassfish.openesb.pojose.core.util.I18n;
import org.glassfish.openesb.pojose.core.util.TransactionHelper;
import org.glassfish.openesb.pojose.core.util.Util;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
public class ConsumerImpl implements Consumer{
    private QName intf;
    private QName svc;
    private ServiceEndpoint se;
    private QName defOprName;
    private QName defInptName;
    private MessageExchange pojosME;
    private MessagingChannel dc;
    private ComponentContext cc;
    private Object transactionMngr;
    private CallTracker pt;

    private static enum ExchangeType {in, out};
    private static final String IN_MSG = "in" ; //NOI18N
    private static final String LoggerName = "org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor" ; //NOI18N

    public ConsumerImpl(MessagingChannel dc, ComponentContext ncc, MessageExchange me){
        this.dc = dc;
        this.cc = ncc;
        this.transactionMngr = ncc.getTransactionManager();
        this.pojosME = me;
    }

    public ConsumerImpl(MessagingChannel dc, ComponentContext ncc, MessageExchange me,
            ServiceEndpoint se, QName opr, QName inpt){
        this.dc = dc;
        this.cc = ncc;
        this.transactionMngr = cc.getTransactionManager();
        this.pojosME = me;
        this.se = se;
        this.defOprName = opr;
        this.defInptName = inpt;
    }

    private ServiceEndpoint lookupServiceEndpointIfNotSet(QName meSvc) throws ErrorMessage{
        ServiceEndpoint se1 = null;
        if (this.se != null){
            se1 = this.se;
        } else {
            if (meSvc != null){
                ServiceEndpoint[] ses = this.cc.getEndpointsForService(meSvc);
                if ((ses != null) && (ses.length > 0)){
                    this.se = ses[0];
                    se1 = this.se;
                }
            } 
            
            if (se1 == null) {
                if (this.svc != null){
                    ServiceEndpoint[] ses = this.cc.getEndpointsForService(this.svc);
                    if ((ses != null) && (ses.length > 0)){
                        this.se = ses[0];
                        se1 = this.se;
                    }else {
                        String m = I18n.loc("POJOSE-7308: No ServiceEndpoint found for the Service set on Consumer {0}.", this.svc);
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m);
                        throw new ErrorMessage(m);
                    }
                } else {
                    String m = I18n.loc("POJOSE-7306: ServiceEndpoint not set, set ServiceEndpoint or Service on Consumer.");
                    Logger.getLogger(LoggerName).log(Level.SEVERE, m);
                    throw new ErrorMessage(m);
                }
            }
        }
        return se1;
    }
    
    public void setProviderME(MessageExchange me){
        this.pojosME = me;
    }

    public void setCallTracker(CallTracker pt){
        this.pt = pt;
    }

    public QName getInterface() {
        return this.intf;
    }

    public QName getService() {
        return this.svc;
    }

    public ServiceEndpoint getServiceEndpoint() {
        return this.se;
    }

    public void setDefaultInputMessageType(QName inpt) {
        this.defInptName = inpt;
    }

    public void setDefaultOperationName(QName opr) {
        this.defOprName = opr;
    }

    public void setInterface(QName intf) {
        this.intf = intf;
    }

    public void setService(QName svc) {
        this.svc = svc;
    }

    public void setServiceEndpoint(ServiceEndpoint se) {
        this.se = se;
    }

    private Object propagetProperties(MessageExchange out, boolean sendTxn) {
        Object suspendedTxn = null;
        if (this.pojosME != null){
            if ((sendTxn) && (this.pojosME.isTransacted())){
                suspendedTxn = TransactionHelper.suspendTransaction(
                        transactionMngr);
                if (suspendedTxn != null){
                    out.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME,
                        this.pojosME.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME));
                }
            }

            NormalizedMessage providerInMessage = this.pojosME.getMessage("in"); //NOI18N
            NormalizedMessage consumingInMessage = out.getMessage("in"); //NOI18N

            Subject sub = providerInMessage.getSecuritySubject();
            if (sub != null){
                consumingInMessage.setSecuritySubject(sub);
            }
        } else {
            // This should never happen
            String m = I18n.loc("POJOSE-7307: Sever internal error: Provider \"in\" MessageExchange is null in ConsumerImpl.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m);
        }

        return suspendedTxn;
    }

    private void resumeTxn(Object txn){
        TransactionHelper.resumeTransaction(transactionMngr, txn);
    }

    /**
     * Note Transaction context propagation is not done for asynchronous mode by default.
     *
     * @param me
     * @throws org.glassfish.openesb.pojose.api.ErrorMessage
     */
    public void send(MessageExchange me) throws ErrorMessage{
        send(me, false);
    }

    public void send(MessageExchange me, boolean propTxn) throws ErrorMessage {
        boolean propagate = propTxn;
        
        if (this.pt.isExecutingOnDone()){
            String msg = I18n.loc("POJOSE-7311: Invoking services in asynchronous mode from OnDone method is not allowed.");
            throw new ErrorMessage(msg);
        }
        
        if (this.pojosME.isTransacted() && propTxn){
            if (!pt.isOkToPropagateTxn(me)) {//Mutable operation.
                propagate = false;
            }
        }

        try {
            propagetProperties(me, propagate);
            ServiceEndpoint se1 = me.getEndpoint();
            if (se1 == null){
                se1 = this.lookupServiceEndpointIfNotSet(me.getService());
                me.setEndpoint(se1);
            }

            MessageExchangeFactory mef = this.dc.createExchangeFactory(se1);
            me.setProperty(ServiceQuality.MESSAGE_ID, this.pt.getUniqueMsgId());
            this.pt.sentAsynchInMsg(pojosME, me);
            this.dc.send(new QoSExchangeTemplate(mef, me));
        } catch (MessagingException mex){
            this.pt.setSendASynchInMsgError(me);
            throw new ErrorMessage(mex);
        }
    }
    
    private MessageExchange sendASynch(ExchangeType type, Object msg, boolean propagateTxn) throws ErrorMessage {
        String m = null;
        boolean processed = true;
        MessageExchange io = null;

        if (this.pt.isExecutingOnDone()){
            m = I18n.loc("POJOSE-7311: Invoking services in asynchronous mode from OnDone method is not allowed.");
            throw new ErrorMessage(m);
        }
        
        try {
            if (type.equals(ExchangeType.in)){
                io = createInOnlyMessageExchange();
            } else {
                io = createInOutMessageExchange();
            }

            if (this.defOprName != null){
                io.setOperation(this.defOprName);
            }

            String inMsgNS = null;
            String inMsgLp = null;
            if (this.defInptName != null){
                inMsgNS = this.defInptName.getNamespaceURI();
                inMsgLp = this.defInptName.getLocalPart();
            }

            if (msg instanceof NormalizedMessage) {
                io.setMessage((NormalizedMessage) msg, IN_MSG);
            } else if (msg instanceof Source) {
                Source src = Util.source2jbiMessage((Source) msg, inMsgNS, inMsgLp);
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setMessage(nm, IN_MSG);
            } else if (msg instanceof Node) {
                Source src = Util.node2WrappedSource((Node) msg, inMsgNS, inMsgLp);
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setMessage(nm, IN_MSG);
            } else if (msg instanceof String) {
                Source src = Util.string2WrappedSource((String) msg, inMsgNS, inMsgLp);
                NormalizedMessage nm = io.createMessage();
                nm.setContent(src);
                io.setMessage(nm, IN_MSG);
            } else {
                //Assume msg is null;
                NormalizedMessage nm = io.createMessage();
                io.setMessage(nm, IN_MSG);
            }

            // QoS Redelivery specific
            ServiceEndpoint se1 = io.getEndpoint();
            if (se1 == null){
                se1 = this.lookupServiceEndpointIfNotSet(io.getService());
                io.setEndpoint(se1);
            }

            MessageExchangeFactory mef = this.dc.createExchangeFactory(se1);
            io.setProperty(ServiceQuality.MESSAGE_ID, this.pt.getUniqueMsgId());

            if (propagateTxn){
                if (!pt.isOkToPropagateTxn(io)){ //Mutable operation
                    propagateTxn = false;
                }
            }
            
            propagetProperties(io, propagateTxn);

            this.pt.sentAsynchInMsg(pojosME, io);
            this.dc.send(new QoSExchangeTemplate(mef, io));
            //this.dc.send(io);
            // QoS Redelivery specific - End
            processed = true;
        } catch (Exception ex) {
            this.pt.setSendASynchInMsgError(io);
            m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
            Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
            throw new ErrorMessage(m, ex);
        }

        if (processed){
            if (io != null){
                if (ExchangeStatus.ERROR.equals(io.getStatus())){
                    m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                    Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                    throw new ErrorMessage(io.getError());
                }
            }
        }
        if (!processed) {
            m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
            throw new ErrorMessage(m);
        }
        
        return io;
    }

    public void sendInOnly(Object msg) throws ErrorMessage {
        sendASynch(ExchangeType.in, msg, false);
    }

    public void sendInOnly(Object msg, boolean propTxn) throws ErrorMessage {
        if (this.pojosME.isTransacted() && propTxn){
            sendASynch(ExchangeType.in, msg, true);
        } else {
            sendASynch(ExchangeType.in, msg, false);
        }
    }

    public void sendInOut(Object msg) throws ErrorMessage {
        sendASynch(ExchangeType.out, msg, false);
    }

    public void sendInOut(Object msg, boolean propTxn) throws ErrorMessage {
        if (this.pojosME.isTransacted() && propTxn){
            sendASynch(ExchangeType.out, msg, true);
        } else {
            sendASynch(ExchangeType.out, msg, false);
        }
    }

    public boolean sendSynch(MessageExchange me) throws MessageException {
        boolean processed = true;
        Object suspTxn = null;
        try {
            if ((me.getEndpoint() == null) && (me.getService() == null)
                    && (me.getInterfaceName() == null)){
                if (this.se != null){
                    me.setEndpoint(this.se);
                } else {
                    if (this.svc != null){
                        me.setService(this.svc);
                    } else {
                        me.setInterfaceName(this.intf);
                    }
                }
            }

            if ((me.getOperation() == null) && (this.defOprName != null)){
                me.setOperation(this.defOprName);
            }

            try {
                suspTxn = propagetProperties(me, true);
                this.pt.updateActiveSynchCalls(true);
                processed = this.dc.sendSync(me);
            } catch (MessagingException ex) {
                Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                throw new ErrorMessage(ex);
            } finally {
                this.pt.updateActiveSynchCalls(false);
            }

            if (processed){
                if (me != null){
                    if (ExchangeStatus.ERROR.equals(me.getStatus())){
                        String m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m, me.getError());
                        throw new ErrorMessage(me.getError());
                    }

                    if (me.getFault() != null){
                        try {
                            // Send done.
                            me.setStatus(ExchangeStatus.DONE);
                            this.dc.send(me);
                        } catch (MessagingException ex) {
                            Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                        }

                        throw new FaultMessage(me.getFault());
                    }
                }
            }
            
            if (!processed) {
                String m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
                throw new ErrorMessage(m);
            }

            if (me instanceof InOut){
                try {
                    me.setStatus(ExchangeStatus.DONE);
                    this.dc.send(me);
                } catch (MessagingException ex){
                    Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                }
            }
        } finally {
            if (suspTxn != null){
                resumeTxn(suspTxn);
            }
        }

        return processed;
    }

    public boolean sendSynch(MessageExchange me, long timeout) throws MessageException {
        boolean processed = true;
        Object suspTxn = null;
        try {
            if ((me.getEndpoint() == null) && (me.getService() == null)
                    && (me.getInterfaceName() == null)){
                if (this.se != null){
                    me.setEndpoint(this.se);
                } else {
                    if (this.svc != null){
                        me.setService(this.svc);
                    } else {
                        me.setInterfaceName(this.intf);
                    }
                }
            }

            if ((me.getOperation() == null) && (this.defOprName != null)){
                me.setOperation(this.defOprName);
            }

            try {
                suspTxn = propagetProperties(me, true);
                this.pt.updateActiveSynchCalls(true);
                processed = this.dc.sendSync(me, timeout);
            } catch (MessagingException ex) {
                Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                throw new ErrorMessage(ex);
            } finally {
                this.pt.updateActiveSynchCalls(false);
            }

            if (processed){
                if (me != null){
                    if (ExchangeStatus.ERROR.equals(me.getStatus())){
                        String m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m, me.getError());
                        throw new ErrorMessage(me.getError());
                    }

                    if (me.getFault() != null){
                        try {
                            // Send done.
                            me.setStatus(ExchangeStatus.DONE);
                            this.dc.send(me);
                        } catch (MessagingException ex) {
                            Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                        }

                        throw new FaultMessage(me.getFault());
                    }
                }
            }

            if (!processed) {
                String m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
                throw new ErrorMessage(m);
            }

            if (me instanceof InOut){
                try {
                    me.setStatus(ExchangeStatus.DONE);
                    this.dc.send(me);
                } catch (MessagingException ex){
                    Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                }
            }
        } finally {
            if (suspTxn != null){
                resumeTxn(suspTxn);
            }
        }


        return processed;
    }
    
    public InOnly createInOnlyMessageExchange() throws MessagingException {
        InOnly io = null;
        MessageExchangeFactory fct = this.dc.createExchangeFactory(se);
        io = fct.createInOnlyExchange();
        
        if (this.se != null){
            io.setEndpoint(this.se);
        } else {
            if (this.svc != null){
                io.setService(this.svc);
            } else {
                io.setInterfaceName(this.intf);
            }
        }

        if (this.defOprName != null){
            io.setOperation(this.defOprName);
        }

        return io;
    }

    public InOut createInOutMessageExchange() throws MessagingException {
        InOut io = null;
        MessageExchangeFactory fct = this.dc.createExchangeFactory(se);
        io = fct.createInOutExchange();

        if (this.se != null){
            io.setEndpoint(this.se);
        } else {
            if (this.svc != null){
                io.setService(this.svc);
            } else {
                io.setInterfaceName(this.intf);
            }
        }

        if (this.defOprName != null){
            io.setOperation(this.defOprName);
        }

        return io;
    }
    
    public void sendSynchInOnly(Object msg) throws ErrorMessage{
        String m = null;
        boolean processed = true;
        InOnly io = null;
        Object suspTxn = null;
        try {
            try {
                MessageExchangeFactory fct = this.dc.createExchangeFactory(se);
                io = fct.createInOnlyExchange();

                if (this.se != null){
                    io.setEndpoint(this.se);
                } else {
                    if (this.svc != null){
                        io.setService(this.svc);
                    } else {
                        io.setInterfaceName(this.intf);
                    }
                }

                if (this.defOprName != null){
                    io.setOperation(this.defOprName);
                }

                String inMsgNS = null;
                String inMsgLp = null;
                if (this.defInptName != null){
                    inMsgNS = this.defInptName.getNamespaceURI();
                    inMsgLp = this.defInptName.getLocalPart();
                }

                if (msg instanceof NormalizedMessage) {
                    io.setInMessage((NormalizedMessage) msg);
                } else if (msg instanceof Source) {
                    Source src = Util.source2jbiMessage((Source) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof Node) {
                    Source src = Util.node2WrappedSource((Node) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof String) {
                    Source src = Util.string2WrappedSource((String) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else {
                    //Assume msg is null;
                    NormalizedMessage nm = io.createMessage();
                    io.setInMessage(nm);
                }

                suspTxn = propagetProperties(io, true);
                this.pt.updateActiveSynchCalls(true);
                processed = this.dc.sendSync(io);
            } catch (Exception ex) {
                m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
                Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
                throw new ErrorMessage(m, ex);
            } finally {
                this.pt.updateActiveSynchCalls(false);
            }

            if (processed){
                if (io != null){
                    if (ExchangeStatus.ERROR.equals(io.getStatus())){
                        m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                        throw new ErrorMessage(io.getError());
                    }
                }
            }
            if (!processed) {
                m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
                throw new ErrorMessage(m);
            }
        }finally {
            if (suspTxn != null){
                resumeTxn(suspTxn);
            }
        }
    }

    public void sendSynchInOnly(Object msg,  long timeout) throws ErrorMessage {
        String m = null;
        boolean processed = true;
        InOnly io = null;
        Object suspTxn = null;
        try {
            try {
                MessageExchangeFactory fct = this.dc.createExchangeFactory(se);
                io = fct.createInOnlyExchange();

                if (this.se != null){
                    io.setEndpoint(this.se);
                } else {
                    if (this.svc != null){
                        io.setService(this.svc);
                    } else {
                        io.setInterfaceName(this.intf);
                    }
                }

                if (this.defOprName != null){
                    io.setOperation(this.defOprName);
                }

                String inMsgNS = null;
                String inMsgLp = null;
                if (this.defInptName != null){
                    inMsgNS = this.defInptName.getNamespaceURI();
                    inMsgLp = this.defInptName.getLocalPart();
                }

                if (msg instanceof NormalizedMessage) {
                    io.setInMessage((NormalizedMessage) msg);
                } else if (msg instanceof Source) {
                    Source src = Util.source2jbiMessage((Source) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof Node) {
                    Source src = Util.node2WrappedSource((Node) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof String) {
                    Source src = Util.string2WrappedSource((String) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else {
                    //Assume msg is null;
                    NormalizedMessage nm = io.createMessage();
                    io.setInMessage(nm);
                }

                suspTxn = propagetProperties(io, true);
                this.pt.updateActiveSynchCalls(true);
                processed = this.dc.sendSync(io, timeout);
            } catch (Exception ex) {
                m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
                Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
                throw new ErrorMessage(m, ex);
            } finally {
                this.pt.updateActiveSynchCalls(false);
            }

            if (processed){
                if (io != null){
                    if (ExchangeStatus.ERROR.equals(io.getStatus())){
                        m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                        throw new ErrorMessage(io.getError());
                    }
                }
            }
            if (!processed) {
                m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
                throw new ErrorMessage(m);
            }
        }finally {
            if (suspTxn != null){
                resumeTxn(suspTxn);
            }
        }
    }
    
    public Object sendSynchInOut(Object msg, MessageObjectType ootype) throws MessageException {
        String m = null;
        InOut io = null;
        boolean processed = true;
        Object ret = null;
        Object suspTxn = null;
        try {
            try {
                MessageExchangeFactory fct = this.dc.createExchangeFactory(se);
                io = fct.createInOutExchange();

                if (this.se != null){
                    io.setEndpoint(this.se);
                } else {
                    if (this.svc != null){
                        io.setService(this.svc);
                    } else {
                        io.setInterfaceName(this.intf);
                    }
                }

                if (this.defOprName != null){
                    io.setOperation(this.defOprName);
                }

                String inMsgNS = null;
                String inMsgLp = null;
                if (this.defInptName != null){
                    inMsgNS = this.defInptName.getNamespaceURI();
                    inMsgLp = this.defInptName.getLocalPart();
                }

                if (msg instanceof NormalizedMessage) {
                    io.setInMessage((NormalizedMessage) msg);
                } else if (msg instanceof Source) {
                    Source src = Util.source2jbiMessage((Source) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof Node) {
                    Source src = Util.node2WrappedSource((Node) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof String) {
                    Source src = Util.string2WrappedSource((String) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else {
                    //Assume msg is null;
                    NormalizedMessage nm = io.createMessage();
                    io.setInMessage(nm);
                }

                suspTxn = propagetProperties(io, true);
                this.pt.updateActiveSynchCalls(true);
                processed = this.dc.sendSync(io);
            } catch (Exception ex) {
                m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
                Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
                throw new ErrorMessage(m, ex);
            } finally {
                this.pt.updateActiveSynchCalls(false);
            }

            if (!processed){
                m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
                throw new ErrorMessage(m);
            }

            if (processed){
                if (io != null){
                    if (ExchangeStatus.ERROR.equals(io.getStatus())){
                        m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                        throw new ErrorMessage(io.getError());
                    }

                    if (io.getFault() != null){
                        try {
                            // Send done.
                            io.setStatus(ExchangeStatus.DONE);
                            this.dc.send(io);
                        } catch (MessagingException ex) {
                            Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                        }

                        throw new FaultMessage(io.getFault());
                    }
                }
            }

            // Now transform out message
            try {
                if (MessageObjectType.NormalizedMessage.equals(ootype)){
                    ret = io.getOutMessage();
                } else if (MessageObjectType.Source.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            src = Util.jbiMessage2Source(src);
                            ret = src;
                        }
                    }
                } else if (MessageObjectType.Document.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            Document doc = Util.jbiMessage2Document(src);
                            ret = doc;
                        }
                    }
                } else if (MessageObjectType.Node.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            Node node = Util.jbiMessage2Node(src);
                            ret = node;
                        }
                    }
                } else if (MessageObjectType.String.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            String str = Util.jbiMessage2String(src);
                            ret = str;
                        }
                    }
                }

                // Send done.
                io.setStatus(ExchangeStatus.DONE);
                this.dc.send(io);

            } catch (Exception ex){
                m = I18n.loc("POJOSE-7303: Exception while transforming Out message.");
                Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
                throw new ErrorMessage(m, ex);
            }
        } finally {
            if (suspTxn != null){
                resumeTxn(suspTxn);
            }
        }

        return ret;
    }

    public Object sendSynchInOut(Object msg, MessageObjectType ootype, long timeout) throws MessageException {
        String m = null;
        InOut io = null;
        boolean processed = true;
        Object ret = null;
        Object suspTxn = null;
        try {
            try {
                MessageExchangeFactory fct = this.dc.createExchangeFactory(se);
                io = fct.createInOutExchange();

                if (this.se != null){
                    io.setEndpoint(this.se);
                } else {
                    if (this.svc != null){
                        io.setService(this.svc);
                    } else {
                        io.setInterfaceName(this.intf);
                    }
                }

                if (this.defOprName != null){
                    io.setOperation(this.defOprName);
                }

                String inMsgNS = null;
                String inMsgLp = null;
                if (this.defInptName != null){
                    inMsgNS = this.defInptName.getNamespaceURI();
                    inMsgLp = this.defInptName.getLocalPart();
                }

                if (msg instanceof NormalizedMessage) {
                    io.setInMessage((NormalizedMessage) msg);
                } else if (msg instanceof Source) {
                    Source src = Util.source2jbiMessage((Source) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof Node) {
                    Source src = Util.node2WrappedSource((Node) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else if (msg instanceof String) {
                    Source src = Util.string2WrappedSource((String) msg, inMsgNS, inMsgLp);
                    NormalizedMessage nm = io.createMessage();
                    nm.setContent(src);
                    io.setInMessage(nm);
                } else {
                    //Assume msg is null;
                    NormalizedMessage nm = io.createMessage();
                    io.setInMessage(nm);
                }

                suspTxn = propagetProperties(io, true);
                this.pt.updateActiveSynchCalls(true);
                processed = this.dc.sendSync(io, timeout);
            } catch (Exception ex) {
                m = I18n.loc("POJOSE-7302: Exception while transforming and sending In message.");
                Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
                throw new ErrorMessage(m, ex);
            } finally {
                this.pt.updateActiveSynchCalls(false);
            }

            if (!processed){
                m = I18n.loc("POJOSE-7301: Delivery Channel failed to deliver the message to the endpoint.");
                throw new ErrorMessage(m);
            }

            if (processed){
                if (io != null){
                    if (ExchangeStatus.ERROR.equals(io.getStatus())){
                        m = I18n.loc("POJOSE-7304: Error while sending a message to the endpoint.");
                        Logger.getLogger(LoggerName).log(Level.SEVERE, m, io.getError());
                        throw new ErrorMessage(io.getError());
                    }

                    if (io.getFault() != null){
                        try {
                            // Send done.
                            io.setStatus(ExchangeStatus.DONE);
                            this.dc.send(io);
                        } catch (MessagingException ex) {
                            Logger.getLogger(LoggerName).log(Level.SEVERE, null, ex);
                        }

                        throw new FaultMessage(io.getFault());
                    }
                }
            }

            // Now transform out message
            try {
                if (MessageObjectType.NormalizedMessage.equals(ootype)){
                    ret = io.getOutMessage();
                } else if (MessageObjectType.Source.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            src = Util.jbiMessage2Source(src);
                            ret = src;
                        }
                    }
                } else if (MessageObjectType.Document.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            Document doc = Util.jbiMessage2Document(src);
                            ret = doc;
                        }
                    }
                } else if (MessageObjectType.Node.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            Node node = Util.jbiMessage2Node(src);
                            ret = node;
                        }
                    }
                } else if (MessageObjectType.String.equals(ootype)){
                    NormalizedMessage nm  = io.getOutMessage();
                    if (nm != null){
                        Source src = nm.getContent();
                        if (src != null){
                            String str = Util.jbiMessage2String(src);
                            ret = str;
                        }
                    }
                }

                // Send done.
                io.setStatus(ExchangeStatus.DONE);
                this.dc.send(io);

            } catch (Exception ex){
                m = I18n.loc("POJOSE-7303: Exception while transforming Out message.");
                Logger.getLogger(LoggerName).log(Level.SEVERE, m, ex);
                throw new ErrorMessage(m, ex);
            }
        } finally {
            if (suspTxn != null){
                resumeTxn(suspTxn);
            }
        }

        return ret;
    }
    
    @Override
    public ConsumerImpl clone(){
        ConsumerImpl cln = new ConsumerImpl(this.dc, this.cc,
                this.pojosME);
        cln.defInptName = this.defInptName;
        cln.defOprName = this.defOprName;
        cln.intf = this.intf;
        cln.se = this.se;
        cln.svc = this.svc;

        return cln;
    }
}
