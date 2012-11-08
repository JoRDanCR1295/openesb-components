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
 * @(#)ValidationVisitor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.core.anno.processor.visitor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.core.anno.processor.Message;
import org.glassfish.openesb.pojose.core.anno.processor.POJOAnnotationProcessor;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyClass;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyMethod;
import org.glassfish.openesb.pojose.core.util.I18n;

/**
 *
 * @author gpatil
 */
public class ValidationVisitor implements Visitor{
    protected Map<Message.MessageType, List<Message>> messages = new HashMap<Message.MessageType, List<Message>>();

    private static Logger logger = Logger.getLogger(POJOAnnotationProcessor.class.getName());

    // Class visitor fields, valid only inside and for each class visit, not outside of that scope.
    private boolean opHasVoidReturn = false;
    private boolean onDoneHasVoidReturn = false;
    private boolean onReplyHasVoidReturn = false;

    public ValidationVisitor(){
        messages.put(Message.MessageType.error, new Vector<Message>());
        messages.put(Message.MessageType.info, new Vector<Message>());
        messages.put(Message.MessageType.warn, new Vector<Message>());
    }

    private void visitClassBegin(){
        this.opHasVoidReturn = false;
        this.onDoneHasVoidReturn = false;
        this.onReplyHasVoidReturn = false;
    }

    /**
     *
     * Validattions.<br>
     * <ul>
     * <li>Method annotated with &amp;#064;Operation, &amp;#064;OReply and &amp;#064;OnDone should be public.
     * <li>POJO class has one and only one method annotated with &amp;#064;Operation and &amp;#064;OnDone.
     * <li>Method annotated with &amp;#064;Operation should takes zero or one input parameter of type String, Node,
     * Source, NormalizedMessage and MessageExchange.
     * <li>Method annotated with &amp;#064;Operation should returns one of void, String, Node,Source, NormalizedMessage
     * and MessageExchange.
     * </ul>
     */
    public void visitClass(ProxyClass c) {
        int opCnt = 0;
        int onDoneCnt = 0;
        int onReplyCnt = 0;
        String msg = null;

        visitClassBegin();

        if ((c.getAnnotationPOJO() != null) || (c.getAnnotationProvider() != null)){
            List<ProxyMethod> ms = c.getMethods();

            for (ProxyMethod m: ms){
                //@Operation
                if (m.getOperationAnnotation() != null){
                    opCnt++;
                    validateOperationMethod(c, m);
                }

                //@OnDone
                if (m.getOnDoneAnnotation() != null){
                    onDoneCnt++;
                    validateOnDoneMethod(c, m);
                }

                //@OnReply
                if (m.getOnReplyAnnotation() != null){
                    onReplyCnt++;
                    validateOnReplyMethod(c, m);
                }
            }

            if (opCnt > 1){
                msg = I18n.loc("POJOSE-7108: More than one method is annotated with @Operation in class {0}.", c.getName());
                addErrorMessage(msg);
                logger.severe(msg);
            }

            if (opCnt == 0){
                msg = I18n.loc("POJOSE-7100: Atleast one public method should be present in class {0} with @Operation annotation.", c.getName());
                addErrorMessage(msg);
                logger.severe(msg);
            }

            if (onReplyCnt > 1){
                msg = I18n.loc("POJOSE-7103: More than one method is annotated with @OnReply in class {0}.", c.getName());
                addErrorMessage(msg);
                logger.severe(msg);
            }

            if (onDoneCnt > 1){
                msg = I18n.loc("POJOSE-7109: More than one method is annotated with @OnDone in class {0}.", c.getName());
                addErrorMessage(msg);
                logger.severe(msg);
            }

            if ( (opCnt > 0) && (onReplyCnt > 0) && (onDoneCnt == 0)){
                msg = I18n.loc("POJOSE-7111: Method annotated with @OnDone is required when asynch call is used in class {0}.", c.getName());
                addErrorMessage(msg);
                logger.severe(msg);
            }

            if ( (opCnt > 0) && (onDoneCnt > 0)){
                if (!this.opHasVoidReturn){
                    msg = I18n.loc("POJOSE-6002: Asynchronous consumer has non void return type for Operation method in class {0}, return type will be ignored. Only return type of OnDone method is considered.", c.getName());
                    addWarningMessage(msg);
                    logger.warning(msg);
                }
            }

            if ( (onReplyCnt > 0) && (onDoneCnt > 0)){
                if (!this.onReplyHasVoidReturn){
                    msg = I18n.loc("POJOSE-6003: Asynchronous consumer has non void return type for OnReply method in class {0}, return type will be ignored. Only return type of OnDone method is considered.", c.getName());
                    addWarningMessage(msg);
                    logger.warning(msg);
                }
            }

            if ((opCnt == 0) && (onReplyCnt > 0)){
                msg = I18n.loc("POJOSE-7113: Method annotated with @OnReply found in class {0} without a public method annotated with @Operation.", c.getName());
                addErrorMessage(msg);
                logger.severe(msg);
            }

            validateFields(c);
        }
    }

    // ***** Non Visitor API methods.
    public List<Message> getMessages(Message.MessageType type){
        return this.messages.get(type);
    }

    public Map<Message.MessageType, List<Message>> getAllMessages(){
        return this.messages;
    }

    protected void addErrorMessage(String m){
        this.messages.get(Message.MessageType.error).add(new Message(Message.MessageType.error, m));
    }

    protected void addWarningMessage(String m){
        this.messages.get(Message.MessageType.warn).add(new Message(Message.MessageType.warn, m));
    }

    protected void addInfoMessage(String m){
        this.messages.get(Message.MessageType.info).add(new Message(Message.MessageType.warn, m));
    }

    // ***** Helper validation methods.

    /**
     *
     * <ul>
     * <li>Method annotated with &amp;#064;Operation should takes zero or one input parameter of type String, Node,
     * Source, NormalizedMessage and MessageExchange.
     * <li>Method annotated with &amp;#064;Operation should returns one of void, String, Node,Source, NormalizedMessage
     * and MessageExchange.
     * </ul>
     * @param cls
     * @param m
     */
    private void validateOperationMethod(ProxyClass c, ProxyMethod m){
        //TODO above validations.
        String msg = null;

        if (!m.isPublic()){
            msg = I18n.loc("POJOSE-7101: Operation method {0} should be public in class {1}.", m.getName(), c.getName());
            addErrorMessage(msg);
            logger.severe(msg);
        }

        if (m.isStatic()){
            msg = I18n.loc("POJOSE-7102: Operation method {0} should not be static in class {1}.", m.getName(), c.getName());
            addErrorMessage(msg);
            logger.severe(msg);
        }

        if (m.getReturnParameterType() == ProxyMethod.ParamType.Void){
            this.opHasVoidReturn = true;
        } else {
            this.opHasVoidReturn = false;
        }
    }

    /**
     *
     * <ul>
     * <li>Method annotated with &amp;#064;OnReply should takes one input parameter of type String, Node,
     * Source, NormalizedMessage and MessageExchange.
     * <li>Method annotated with &amp;#064;OnReply should returns void.
     * </ul>
     * @param cls
     * @param m
     */
    private void validateOnReplyMethod(ProxyClass c, ProxyMethod m){
        //TODO above validations.
        String msg = null;

        if (!m.isPublic()){
            msg = I18n.loc("POJOSE-7107: OnReply method {0} should be public in class {1}.", m.getName(), c.getName());
            addErrorMessage(msg);
            logger.severe(msg);
        }

        if (m.isStatic()){
            msg = I18n.loc("POJOSE-7110: OnReply annotated method {0} should not be static in class {1}.", m.getName(), c.getName());
            addErrorMessage(msg);
            logger.severe(msg);
        }

        if (m.getReturnParameterType() == ProxyMethod.ParamType.Void){
            this.onReplyHasVoidReturn = true;
        } else {
            this.onReplyHasVoidReturn = false;
        }
    }

    /**
     *
     * <ul>
     * <li>Method annotated with &amp;#064;OnDone should takes no input parameter.
     * <li>Method annotated with &amp;#064;OnDone should returns one of type
     * void, String, Node, Source, NormalizedMessage and MessageExchange.
     * </ul>
     * @param cls
     * @param m
     */
    private void validateOnDoneMethod(ProxyClass c, ProxyMethod m){
        //TODO above validations.
        String msg = null;

        if (!m.isPublic()){
            msg = I18n.loc("POJOSE-7106: OnDone annotated method {0} should be public in class {1}.", m.getName(), c.getName());
            addErrorMessage(msg);
            logger.severe(msg);
        }

        if (m.isStatic()){
            msg = I18n.loc("POJOSE-7112: OnDone annotated method {0} should not be static in class {1}.", m.getName(), c.getName());
            addErrorMessage(msg);
            logger.severe(msg);
        }

        if (m.getReturnParameterType() == ProxyMethod.ParamType.Void){
            this.onDoneHasVoidReturn = true;
        } else {
            this.onDoneHasVoidReturn = false;
        }

    }

    /**
     * Resource only on PojoContext.
     * Endpoint only on ServiceEndpoint.
     * ServiceEndpoint matches with specific OnReplys.
     * @param cls
     */
    private void validateFields(ProxyClass cls){
        //TODO
    }
}
