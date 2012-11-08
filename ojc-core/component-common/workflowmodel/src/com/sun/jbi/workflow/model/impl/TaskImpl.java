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
 * @(#)TaskImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathContext;
import org.w3c.dom.Attr;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import com.sun.jbi.workflow.model.Action;
import com.sun.jbi.workflow.model.Assignment;
import com.sun.jbi.workflow.model.DeadlineOrDuration;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.Init;
import com.sun.jbi.workflow.model.Keyword;
import com.sun.jbi.workflow.model.Keywords;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.Timeout;
import com.sun.jbi.workflow.model.utl.Messages;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TAction;
import com.sun.jbi.workflow.model.xmlbeans.TEscalation;
import com.sun.jbi.workflow.model.xmlbeans.TImport;
import com.sun.jbi.workflow.model.xmlbeans.TInit;
import com.sun.jbi.workflow.model.xmlbeans.TKeywords;
import com.sun.jbi.workflow.model.xmlbeans.TNotification;
import com.sun.jbi.workflow.model.xmlbeans.TTask;
import com.sun.jbi.workflow.model.xmlbeans.TTimeout;
import com.sun.jbi.workflow.model.xmlbeans.TActionType.Enum;

public class TaskImpl extends ModelElementImpl implements Task {
    
    private static final Messages MESSAGES = Messages
    .getMessages(TaskImpl.class);    

    private TTask mTaskType;

    private Operation mOperation;

    private Init mInit;

    private Assignment mAssignment;

    private List<Timeout> mTimeoutList = new ArrayList<Timeout>();

    private List<Escalation> mEscalationList = new ArrayList<Escalation>();

    private List<Notification> mNotificationList = new ArrayList<Notification>();

    private List<Action> mActionList = new ArrayList<Action>();
    
    private   PortType mPortType = null;
    
    private String mBaseURI;
    
    private Map<String, String> mNamespaceMap = null;    
    
    private List<Definition> mImportWSDLs;
    
    private QName mQName;
    
    private List<Keyword> mKeywords ;

    public TaskImpl(TTask taskType, String baseURI) {
        super(taskType, null);
        mTaskType = taskType;
        mBaseURI = baseURI;
        mNamespaceMap = new HashMap<String, String>();
        NamedNodeMap nodeMap = getDelegate().getDomNode().getAttributes();
        if (nodeMap.getLength() > 0) {
            for (int i = 0; i < nodeMap.getLength(); i++) {
                Attr attribute = Attr.class.cast(nodeMap.item(i));
                String name = attribute.getName();
                ModelUtil.addToNSMap(name, attribute.getValue(), mNamespaceMap);
            }

        }         
        init();
    }

    private void init() {        

        mQName = new QName (mTaskType.getTargetNamespace(), mTaskType.getName());
        mPortType = Util.getPortType(mTaskType.getPortType(), this);
        this.mOperation = Util.getWSDLOperation(mPortType, this.mTaskType.getOperation(),
                this);

        if (mTaskType.getAssignment() != null) {
            mAssignment = new AssignmentImpl(mTaskType.getAssignment(), this);
        }
        List<TTimeout> tList = mTaskType.getTimeoutList();
        if (tList != null) {
            Iterator<TTimeout> it = tList.iterator();
            while (it.hasNext()) {
                TTimeout tt = it.next();
                Timeout t = new TimeoutImpl(tt, this);
                this.mTimeoutList.add(t);
            }
        }
        List<TEscalation> eList = mTaskType.getEscalationList();
        if (eList != null) {
            Iterator<TEscalation> eit = eList.iterator();
            while (eit.hasNext()) {
                TEscalation et = eit.next();
                Escalation e = new EscalationImpl(et, this);
                this.mEscalationList.add(e);
            }
        }

        List<TNotification> eNtList = mTaskType.getNotificationList();
        if (eNtList != null) {
            Iterator<TNotification> nit = eNtList.iterator();
            while (nit.hasNext()) {
                TNotification emailNotfication = nit.next();                
//                if (et.getType() == TNotificationType.EMAIL) {
//                    TEmailNotification emailNotfication = null;
//                    try {
//                         emailNotfication = TEmailNotification.Factory.parse(et.getDomNode());
//                    }catch (XmlException e) {
//                            // TODO Auto-generated catch block
//                            throw new ModelException (MESSAGES.getString("ModelFactory.Parse_xmlException",mBaseURI));
//                        }         
                    Notification n = new EmailNotificationImpl(emailNotfication, this);
                this.mNotificationList.add(n);
//            }
        }
        }

        List<TAction> eAcList = mTaskType.getActionList();
        if (eAcList != null) {
            Iterator<TAction> acit = eAcList.iterator();
            while (acit.hasNext()) {
                TAction act = acit.next();
                Action action = new ActionImpl(act, this);
                this.mActionList.add(action);
            }
        }

        TInit init = mTaskType.getInit();
        if (init != null) {
            mInit = new InitImpl(init, this);
        }
        
        TKeywords keywords = mTaskType.getKeywords();
        if (keywords != null && keywords.getKeywordList() != null && keywords.getKeywordList().size() >0) {
            Keywords keywordsImpl = new KeywordsImpl (keywords, this);
            mKeywords = keywordsImpl.getKeywords();
        } else {
            mKeywords = null;
        }
    }

    public String getName() {
        return this.mTaskType.getName();
    }

    public Operation getWSDLOperation() throws ModelException {
        if (mOperation != null) {
            return mOperation;
        }

        this.mOperation = Util.getWSDLOperation(getPortType(), this.mTaskType.getOperation(),
                this);
        return mOperation;
    }


    public PortType getPortType() throws ModelException {
        return mPortType;
    }

    public Assignment getTaskAssignment() {

        return mAssignment;
    }

    public List<Timeout> getTaskTimeouts() {

        return this.mTimeoutList;
    }

    public List<Escalation> getTaskEscalations() {
        return this.mEscalationList;
    }

    public List<Notification> getTaskNotifications() {
        return this.mNotificationList;
    }

    public List<Action> getTaskActions() {

        return this.mActionList;
    }

    public List<ModelElement> getChildren() {
        List<ModelElement> children = new ArrayList<ModelElement>();
        children.add(getTaskAssignment());
        children.addAll(getTaskTimeouts());
        children.addAll(getTaskEscalations());

        return children;
    }

    public DeadlineOrDuration findDeadlineOrDuration(String xpath) {
        DeadlineOrDuration deadlineOrDuration = null;

        List<Timeout> timeouts = getTaskTimeouts();
        Iterator<Timeout> it1 = timeouts.iterator();
        while (it1.hasNext()) {
            Timeout t = it1.next();
            if (xpath.equals(t.getXPathInfo().getXPath())) {
                deadlineOrDuration = t;
                return deadlineOrDuration;
            }
        }

        List<Escalation> escalations = getTaskEscalations();
        Iterator<Escalation> it2 = escalations.iterator();
        while (it2.hasNext()) {
            Escalation e = it2.next();
            if (xpath.equals(e.getXPathInfo().getXPath())) {
                deadlineOrDuration = e;
                return deadlineOrDuration;
            }
        }
        return deadlineOrDuration;
    }

    public List<Definition> getImportWSDLs() throws ModelException {
        // TODO Auto-generated method stub
        if (mImportWSDLs != null) {
            return mImportWSDLs;
        }
        try {
            WSDLFactory wsdlFactory = WSDLFactory.newInstance();
            WSDLReader wsdlReader = wsdlFactory.newWSDLReader();
            wsdlReader.setFeature("javax.wsdl.importDocuments", true);

            List<TImport> imports = this.mTaskType.getImportList();

            if (imports != null && imports.size() > 0) {
                mImportWSDLs = new ArrayList<Definition>(imports.size());
                for (int i = 0; i < imports.size(); i++) {
                    TImport importType = imports.get(i);
                    Definition definition = wsdlReader.readWSDL(mBaseURI,
                            importType.getLocation());
                     mImportWSDLs.add(definition);
                }
            }
        } catch (Exception e) {
            throw new ModelException(MESSAGES
                    .getString("TasksImpl.Can_not_parse_WSDL"), e);
        }
        return mImportWSDLs;
    }


    public Init getInit() {
        // TODO Auto-generated method stub

        return mInit;
    }

    public Notification getNotification(String notificationQName) {
        Notification notification = null;
        if (notificationQName != null) {

            List<Notification> nList = this.getTaskNotifications();
            Iterator<Notification> it = nList.iterator();
            while (it.hasNext()) {
                Notification n = it.next();
                String qName = n.getName();
                if (qName.equals(notificationQName)) {
                    notification = n;
                    break;
                }
            }

        }

        return notification;
    }
    
    public String getTargetNamespace() {
        return this.mTaskType.getTargetNamespace();
    }
    
    public Map<String, String> getTotalNamespaces() {
        // TODO Auto-generated method stub
        return mNamespaceMap;
    }

    public String getOptName() {
        // TODO Auto-generated method stub
        return mTaskType.getOperation();
    }

    public QName getPortTypeQName() {
        // TODO Auto-generated method stub
        return mTaskType.getPortType();
    }

    public QName getQName() {
        // TODO Auto-generated method stub
        return mQName;
    }

    public int getPriority(JXPathContext context) throws ModelException {
        // TODO Auto-generated method stub
        int returnVal = 5;
        String strVal = ModelUtil.getExpressionContent(context, mTaskType
                .getPriority(), this, "");
        if (strVal != null) {
            try {
                returnVal = Integer.parseInt(strVal);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return returnVal;
    }

    public String getTitle(JXPathContext context) throws ModelException {
        return ModelUtil.getExpressionContent(context, mTaskType
                .getTitle(), this, "");
//        boolean isXpath20 = false;
//        if ( mTaskType.getTitle().getExpressionLanguage() != null && mTaskType.getTitle().getExpressionLanguage().equals(XPATH20)) {
//            isXpath20 = true;
//        }         
//        // TODO Auto-generated method stub
//        Node domNode = mTaskType.getTitle().getDomNode();
//        String text = null;
//        if (domNode != null) {
//            text = ModelUtil.getText(domNode);
//            String strVal = null;
//            try {
//                if (isXpath20) {
//                    strVal = ModelUtil.getStringValueXpath20(context, text, getTask(), "");
//                }  else {                
//                    strVal = ModelUtil.getStringValue(context, text, "");
//                }
//            } catch (Exception e) {
//                // TODO Auto-generated catch block
//                throw new ModelException(e);
//            }
//            return strVal;
//        }
//        return null;
    }

    public Action getAction(Enum type) {
        // TODO Auto-generated method stub
        for (Action action : mActionList) {
            if (action.getType() == type) {
                return action;
            }
        }
        return null;
    }

    public List<Keyword> getKeywords() {
        // TODO Auto-generated method stub
        return mKeywords;
    }


}
