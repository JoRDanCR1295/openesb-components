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
 * @(#)BasePojoExecutor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.nmr;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.api.MessageException;
import org.glassfish.openesb.pojose.core.anno.meta.OperationMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.jbi.POJOComponentContext;
import org.glassfish.openesb.pojose.core.util.Constants;
import org.glassfish.openesb.pojose.core.util.TransactionHelper;
import org.glassfish.openesb.pojose.core.util.Util;
import org.glassfish.openesb.pojose.jbi.su.MasterTracker;
import org.glassfish.openesb.pojose.jbi.su.ProviderTracker;
import org.glassfish.openesb.pojose.res.impl.CallTracker;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */
public abstract class BasePojoExecutor implements POJOExecutor {

    private static final String IN_MSG = "in"; //NOI18N
    private static final String OUT_MSG = "out"; //NOI18N
    protected POJOClassMetadata meta;
    protected OperationMetadata opm;
    protected POJOComponentContext ctx;
    protected Object pojo;
    private static Logger logger = Logger.getLogger(BasePojoExecutor.class.getName());

    public BasePojoExecutor(POJOComponentContext ctx, POJOClassMetadata classMeta,
            Object pojo) {
        this.ctx = ctx;
        this.meta = classMeta;
        this.opm = classMeta.getOperationMetadata();
        this.pojo = pojo;
    }

    protected Object[] getInputArguments(MessageExchange me, Method m,
            OperationMetadata opm) throws Exception {
        Class[] pt = m.getParameterTypes();
        if (pt.length == 1) {
            Class fp = pt[0];
            Object[] ret = new Object[1];
            NormalizedMessage inMsg = me.getMessage(IN_MSG);

            if (fp.isAssignableFrom(MessageExchange.class)) {
                // Argument is MessageExchange
                ret[0] = me;
            } else if (fp.isAssignableFrom(NormalizedMessage.class)) {
                // Argument is NormalizedMessage
                ret[0] = inMsg;
            } else if (fp.isAssignableFrom(Source.class)) {
                // Argument is Source
                ret[0] = Util.jbiMessage2Source(inMsg.getContent());
            } else if (fp.isAssignableFrom(Node.class)) {
                // Argument is Node                
                ret[0] = Util.jbiMessage2Node(inMsg.getContent());
            } else if (fp.getName().equals(Document.class.getName())) {
                // Argument is Document
                ret[0] = Util.jbiMessage2Document(inMsg.getContent());
            } else if (fp.isAssignableFrom(String.class)) {
                // Argument is String
                ret[0] = Util.jbiMessage2String(inMsg.getContent());
            } else if (fp.isAssignableFrom(byte[].class)) {
                // Argument is byte[]
            } else {
                Annotation ann = fp.getAnnotation(XmlRootElement.class);
                if (ann != null) {
                    JAXBContext jaxbc = JAXBContext.newInstance(fp);
                    ret[0] = jaxbc.createUnmarshaller().unmarshal(
                            Util.jbiMessage2Node(inMsg.getContent()));
                }
            }
            return ret;
        } else if (pt.length > 1) {
            // just pass all argument as null.
            Object[] ret = new Object[pt.length];
            return ret;
        }

        return null;
    }

    protected Object[] getOnReplyInputArguments(MessageExchange meo, Method m)
            throws Exception {
        Class[] pt = m.getParameterTypes();
        InOut me = (InOut) meo;
        if (pt.length == 2) {
            Object[] ret = new Object[2];
            Class fp = pt[1];
            // TODO validate first parameter must be of type ServiceEndpoint
            ret[0] = me.getEndpoint();

            NormalizedMessage outMsg = me.getOutMessage();

            if (fp.isAssignableFrom(MessageExchange.class)) {
                // Argument is MessageExchange
                ret[1] = me;
            } else if (fp.isAssignableFrom(NormalizedMessage.class)) {
                // Argument is NormalizedMessage
                ret[1] = outMsg;
            } else if (fp.isAssignableFrom(Source.class)) {
                // Argument is Source
                ret[1] = Util.jbiMessage2Source(outMsg.getContent());
            } else if (fp.isAssignableFrom(Node.class)) {
                // Argument is Node
                ret[1] = Util.jbiMessage2Node(outMsg.getContent());
            } else if (fp.getName().equals(Document.class.getName())) {
                // Argument is Document
                ret[1] = Util.jbiMessage2Document(outMsg.getContent());
            } else if (fp.isAssignableFrom(String.class)) {
                // Argument is String
                ret[1] = Util.jbiMessage2String(outMsg.getContent());
            } else if (fp.isAssignableFrom(byte[].class)) {
                // Argument is byte[]
            } else {
                Annotation ann = fp.getAnnotation(XmlRootElement.class);
                if (ann != null) {
                    JAXBContext jaxbc = JAXBContext.newInstance(fp);
                    ret[0] = jaxbc.createUnmarshaller().unmarshal(
                            Util.jbiMessage2Node(outMsg.getContent()));
                }
            }
            return ret;
        } else if (pt.length > 2) {
            // just pass all argument as null.
            Object[] ret = new Object[pt.length];
            return ret;
        }

        return null;
    }

    protected Object[] getOnErrorInputArguments(MessageExchange me, Method m)
            throws Exception {
        Class[] pt = m.getParameterTypes();
        if (pt.length == 2) {
            Object[] ret = new Object[2];
            Class fp = pt[1];
            // TODO validate first parameter must be of type ServiceEndpoint
            ret[0] = me.getEndpoint();

            if (fp.isAssignableFrom(MessageExchange.class)) {
                // Argument is MessageExchange
                ret[1] = me;
            } else if (fp.isAssignableFrom(Exception.class)) {
                // Argument is NormalizedMessage
                ret[1] = me.getError();
            }
            return ret;
        } else if (pt.length > 2) {
            // just pass all argument as null.
            Object[] ret = new Object[pt.length];
            return ret;
        }

        return null;
    }

    protected Object[] getOnFaultInputArguments(MessageExchange me, Method m)
            throws Exception {
        Class[] pt = m.getParameterTypes();
        if (pt.length == 2) {
            Object[] ret = new Object[2];
            Class fp = pt[1];
            // TODO validate first parameter must be of type ServiceEndpoint
            ret[0] = me.getEndpoint();
            ret[1] = me;

            return ret;
        } else if (pt.length > 2) {
            // just pass all argument as null.
            Object[] ret = new Object[pt.length];
            return ret;
        }

        return null;
    }

    protected void updateOutMessage(MessageExchange me, Object ret,
            OperationMetadata opm) {
        this.updateOutMessage(me, ret, opm, opm.getMethod().getReturnType());
    }

    protected void updateOutMessage(MessageExchange me, Object ret,
            OperationMetadata opm, Method onDone) throws Exception {
        this.updateOutMessage(me, ret, opm, onDone.getReturnType());
    }

    protected void updateOutMessage(MessageExchange me, Object ret,
            OperationMetadata opm, Class retType) {

        try {
            if (retType == Void.TYPE) {
                me.setStatus(ExchangeStatus.DONE);
            } else if (MessageExchange.class.isAssignableFrom(retType)) {
                //nop
            } else if (NormalizedMessage.class.isAssignableFrom(retType)) {
                NormalizedMessage out = (NormalizedMessage) ret;
                me.setMessage(out, OUT_MSG);
            } else if (Source.class.isAssignableFrom(retType)) {
                Source src = (Source) ret;
                Source retSrc = null;

                if (!Constants.ANNOTATION_NULL_VAL.equals(opm.getOperation()
                        .outMessageTypeQN())) {
                    QName qn = QName.valueOf(opm.getOperation()
                            .outMessageTypeQN());
                    retSrc = Util.source2jbiMessage(src, qn.getNamespaceURI(),
                            qn.getLocalPart());
                } else {
                    if ((!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageType()))
                            || (!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageTypeNS()))) {
                        retSrc = Util.source2jbiMessage(src,
                                opm.getOperation().outMessageTypeNS(),
                                opm.getOperation().outMessageType());
                    } else {
                        retSrc = Util.source2jbiMessage(src);
                    }
                }

                NormalizedMessage out = me.createMessage();
                out.setContent(retSrc);
                me.setMessage(out, OUT_MSG);
            } else if (Document.class.isAssignableFrom(retType)) {
                Document node = (Document) ret;
                Source retSrc = null;

                if (!Constants.ANNOTATION_NULL_VAL.equals(opm.getOperation()
                        .outMessageTypeQN())) {
                    QName qn = QName.valueOf(opm.getOperation()
                            .outMessageTypeQN());
                    retSrc = Util.doc2WrappedSource(node, qn.getNamespaceURI(),
                            qn.getLocalPart());
                } else {
                    if ((!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageType()))
                            || (!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageTypeNS()))) {
                        retSrc = Util.doc2WrappedSource(node,
                                opm.getOperation().outMessageTypeNS(),
                                opm.getOperation().outMessageType());
                    } else {
                        retSrc = Util.doc2WrappedSource(node);
                    }
                }
                NormalizedMessage out = me.createMessage();
                out.setContent(retSrc);
                me.setMessage(out, OUT_MSG);
            } else if (Node.class.isAssignableFrom(retType)) {
                Node node = (Node) ret;
                Source retSrc = null;

                if (!Constants.ANNOTATION_NULL_VAL.equals(opm.getOperation()
                        .outMessageTypeQN())) {
                    QName qn = QName.valueOf(opm.getOperation()
                            .outMessageTypeQN());
                    retSrc = Util.node2WrappedSource(node, qn.getNamespaceURI(),
                            qn.getLocalPart());
                } else {
                    if ((!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageType()))
                            || (!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageTypeNS()))) {
                        retSrc = Util.node2WrappedSource(node,
                                opm.getOperation().outMessageTypeNS(),
                                opm.getOperation().outMessageType());
                    } else {
                        retSrc = Util.node2WrappedSource(node);
                    }
                }
                NormalizedMessage out = me.createMessage();
                out.setContent(retSrc);
                me.setMessage(out, OUT_MSG);
            } else if (String.class.isAssignableFrom(retType)) {
                String outStr = (String) ret;
                Source retSrc = null;
                if (!Constants.ANNOTATION_NULL_VAL.equals(opm.getOperation()
                        .outMessageTypeQN())) {
                    QName qn = QName.valueOf(opm.getOperation()
                            .outMessageTypeQN());
                    retSrc = Util.string2WrappedSource(outStr, qn.getNamespaceURI(),
                            qn.getLocalPart());
                } else {
                    if ((!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageType()))
                            || (!Constants.ANNOTATION_NULL_VAL.
                            equals(opm.getOperation().outMessageTypeNS()))) {
                        retSrc = Util.string2WrappedSource(outStr,
                                opm.getOperation().outMessageTypeNS(),
                                opm.getOperation().outMessageType());
                    } else {
                        retSrc = Util.string2WrapperdSource(outStr);
                    }
                }
                NormalizedMessage out = me.createMessage();
                out.setContent(retSrc);
                me.setMessage(out, OUT_MSG);
            } else {
                Annotation ann = retType.getAnnotation(XmlRootElement.class);
                if (ann != null) {
                    JAXBContext jaxbc = JAXBContext.newInstance(retType);
                    DocumentBuilderFactory factory =
                            DocumentBuilderFactory.newInstance();
                    DocumentBuilder builder =
                            factory.newDocumentBuilder();
                    Document node = builder.newDocument();
                    jaxbc.createMarshaller().marshal(ret, node);

                    Source retSrc = null;

                    if (!Constants.ANNOTATION_NULL_VAL.equals(opm.getOperation()
                            .outMessageTypeQN())) {
                        QName qn = QName.valueOf(opm.getOperation()
                                .outMessageTypeQN());
                        retSrc = Util.doc2WrappedSource(node, qn.getNamespaceURI(),
                                qn.getLocalPart());
                    } else {
                        if ((!Constants.ANNOTATION_NULL_VAL.
                                equals(opm.getOperation().outMessageType()))
                                || (!Constants.ANNOTATION_NULL_VAL.
                                equals(opm.getOperation().outMessageTypeNS()))) {
                            retSrc = Util.doc2WrappedSource(node,
                                    opm.getOperation().outMessageTypeNS(),
                                    opm.getOperation().outMessageType());
                        } else {
                            retSrc = Util.doc2WrappedSource(node);
                        }
                    }
                    NormalizedMessage out = me.createMessage();
                    out.setContent(retSrc);
                    me.setMessage(out, OUT_MSG);
                }
            }
        } catch (Exception ex) {
            logger.log(Level.SEVERE, null, ex);
            try {
                me.setError(ex);
            } catch (Exception ex1) {
                logger.log(Level.SEVERE, null, ex1);
            }
        }
    }

    /**
     * OnReply has to be present for InOut asynch service consume call.
     *
     * @param provME
     * @param consME
     */
    public void executeOnReply(MessageExchange provME, MessageExchange consME) {
        Exception exp = null;

        ProviderTracker pt = MasterTracker.getInstance().getProviderTracker(provME);
        ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();

        try {
            Method onReply = this.meta.getOnReplyMetadata().getMethod();
            // Ctx CL
            Thread.currentThread().setContextClassLoader(pojo.getClass().getClassLoader());
            // Ctx Txn. Set the Transaction context.
            if (provME.isTransacted()) {
                TransactionHelper.checkAndResumeForConsumedME(ctx.getJBIComponentContext().getTransactionManager(),
                        provME.getProperty(provME.JTA_TRANSACTION_PROPERTY_NAME), consME, (CallTracker) pt);
            }

            Object[] args = getOnReplyInputArguments(consME, onReply);
            onReply.invoke(pojo, args);
        } catch (InvocationTargetException ite) {
            Throwable cause = ite.getCause();
            if (cause instanceof ErrorMessage) {
                exp = (ErrorMessage) cause;
            } else if (cause instanceof FaultMessage) {
                exp = (MessageException) cause;
            } else if (cause instanceof Exception) {
                exp = (Exception) cause;
            } else {
                exp = new Exception(cause);
            }
            logger.log(Level.SEVERE, null, cause);
        } catch (Throwable ex) {
            if (ex instanceof Exception) {
                exp = (Exception) ex;
            } else {
                exp = new Exception(ex);
            }
        } finally {
            // Ctx CL
            if (thdCtxCl != null) {
                Thread.currentThread().setContextClassLoader(thdCtxCl);
            }

            // Ctx Txn
            if (provME.isTransacted()) {
                TransactionHelper.suspendIfStartedForConsumedME(
                        ctx.getJBIComponentContext().getTransactionManager(),
                        consME, (CallTracker) pt);
            }
        }

        try {
            consME.setStatus(ExchangeStatus.DONE);
            ctx.getDC().send(consME);
            if (logger.isLoggable(Level.FINEST)) {
                logger.finest("Sent DONE for consumed MEX:" + consME.getExchangeId()); //NOI18N
            }

        } catch (MessagingException ex) {
            logger.log(Level.SEVERE, null, ex);
        } finally {
            pt.setExecutedOnReply(consME);
        }

        try {
            if (exp == null) {
                // Valid onDone is present.
                if (pt.canExecuteOnDone()) {
                    if (pt.tryAcquireLockForOnDoneExec()) {
                        try {
                            //##### TODO Remove ME
                            //Exception ex = new Exception("Got lock to execute OnDone!!!");
                            //ex.printStackTrace();
                            //##### TODO Remove ME - End

                            this.executeOnDone(provME);
                        } finally {
                            pt.releaseLockForOnDoneExec();
                        }
                    }
                }
            } else {
                provME.setError(exp);
                ctx.getDC().send(provME);
            }
        } catch (Exception ex) {
            exp = ex;
            logger.log(Level.SEVERE, null, ex);
        } finally {
            if (exp != null) {
                pt.setDoneWithProvisioning();
            }
        }
    }

    /**
     * If OnError not be present, log the message.
     *
     * @param provME
     * @param consME
     */
    public void executeOnError(MessageExchange provME, MessageExchange consME) {
        Exception exp = null;

        ProviderTracker pt = MasterTracker.getInstance().getProviderTracker(provME);
        ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();

        if ((this.meta.getOnErrorMetadata() != null)
                && (this.meta.getOnErrorMetadata().getMethod() != null)) {
            try {
                Method onError = this.meta.getOnErrorMetadata().getMethod();
                // Ctx CL
                Thread.currentThread().setContextClassLoader(pojo.getClass().getClassLoader());

                // Ctx Txn. Set the Transaction context.
                if (provME.isTransacted()) {
                    TransactionHelper.checkAndResumeForConsumedME(ctx.getJBIComponentContext().getTransactionManager(),
                            provME.getProperty(provME.JTA_TRANSACTION_PROPERTY_NAME), consME, (CallTracker) pt);
                }

                Object[] args = getOnErrorInputArguments(consME, onError);
                onError.invoke(pojo, args);
            } catch (InvocationTargetException ite) {
                Throwable cause = ite.getCause();
                if (cause instanceof ErrorMessage) {
                    exp = (ErrorMessage) cause;
                } else if (cause instanceof FaultMessage) {
                    exp = (MessageException) cause;
                } else if (cause instanceof Exception) {
                    exp = (Exception) cause;
                } else {
                    exp = new Exception(cause);
                }
                logger.log(Level.SEVERE, null, cause);
            } catch (Exception ex) {
                exp = ex;
            } finally {
                // Ctx CL
                if (thdCtxCl != null) {
                    Thread.currentThread().setContextClassLoader(thdCtxCl);
                }

                // Ctx Txn
                if (provME.isTransacted()) {
                    TransactionHelper.suspendIfStartedForConsumedME(
                            ctx.getJBIComponentContext().getTransactionManager(),
                            consME, (CallTracker) pt);
                }

                pt.setExecutedOnError(consME);
            }
        } else {
            // XXX TODO Log using I18N
            logger.log(Level.SEVERE, null, consME.getError());
            pt.setExecutedOnError(consME);
        }

        try {
            if (exp == null) {
                // Valid onDone is present.
                if (pt.canExecuteOnDone()) {
                    if (pt.tryAcquireLockForOnDoneExec()) {
                        try {
                            this.executeOnDone(provME);
                        } finally {
                            pt.releaseLockForOnDoneExec();
                        }
                    }
                }
            } else {
                provME.setError(exp);
                ctx.getDC().send(provME);
            }
        } catch (Exception ex) {
            exp = ex;
            logger.log(Level.SEVERE, null, ex);
        } finally {
            if (exp != null) {
                pt.setDoneWithProvisioning();
            }
        }
    }

    public void executeOnFault(MessageExchange provME, MessageExchange consME) {
        Exception exp = null;

        ProviderTracker pt = MasterTracker.getInstance().getProviderTracker(provME);
        ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();

        if ((this.meta.getOnFaultMetadata() != null)
                && (this.meta.getOnFaultMetadata().getMethod() != null)) {
            try {
                Method onFault = this.meta.getOnFaultMetadata().getMethod();
                // Ctx CL
                Thread.currentThread().setContextClassLoader(pojo.getClass().getClassLoader());

                // Ctx Txn. Set the Transaction context.
                if (provME.isTransacted()) {
                    TransactionHelper.checkAndResumeForConsumedME(ctx.getJBIComponentContext().getTransactionManager(),
                            provME.getProperty(provME.JTA_TRANSACTION_PROPERTY_NAME), consME, (CallTracker) pt);
                }

                Object[] args = getOnFaultInputArguments(consME, onFault);
                onFault.invoke(pojo, args);
            } catch (InvocationTargetException ite) {
                Throwable cause = ite.getCause();
                if (cause instanceof ErrorMessage) {
                    exp = (ErrorMessage) cause;
                } else if (cause instanceof FaultMessage) {
                    exp = (MessageException) cause;
                } else if (cause instanceof Exception) {
                    exp = (Exception) cause;
                } else {
                    exp = new Exception(cause);
                }
                logger.log(Level.SEVERE, null, cause);
            } catch (Exception ex) {
                exp = ex;
            } finally {
                // Ctx CL
                if (thdCtxCl != null) {
                    Thread.currentThread().setContextClassLoader(thdCtxCl);
                }

                // Ctx Txn
                if (provME.isTransacted()) {
                    TransactionHelper.suspendIfStartedForConsumedME(
                            ctx.getJBIComponentContext().getTransactionManager(),
                            consME, (CallTracker) pt);
                }
            }
        } else {
            // XXX TODO No OnError method, log warn using I18N
            logger.log(Level.SEVERE, null, consME.getFault());
        }

        try {
            consME.setStatus(ExchangeStatus.DONE);
            ctx.getDC().send(consME);
        } catch (MessagingException ex) {
            logger.log(Level.SEVERE, null, ex);
        } finally {
            pt.setExecutedOnFault(consME);
        }

        try {
            if (exp == null) {
                // Valid onDone is present.
                if (pt.canExecuteOnDone()) {
                    if (pt.tryAcquireLockForOnDoneExec()) {
                        try {
                            this.executeOnDone(provME);
                        } finally {
                            pt.releaseLockForOnDoneExec();
                        }
                    }
                }
            } else {
                provME.setError(exp);
                ctx.getDC().send(provME);
            }
        } catch (Exception ex) {
            exp = ex;
            logger.log(Level.SEVERE, null, ex);
        } finally {
            if (exp != null) {
                pt.setDoneWithProvisioning();
            }
        }
    }
}
