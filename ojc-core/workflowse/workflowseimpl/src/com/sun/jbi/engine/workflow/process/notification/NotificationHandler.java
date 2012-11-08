package com.sun.jbi.engine.workflow.process.notification;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.workflow.process.Handler;
import com.sun.jbi.engine.workflow.process.TaskHandlerManager;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskStateEvent;
import com.sun.jbi.engine.workflow.runtime.model.TaskStateListener;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.Address;
import com.sun.jbi.workflow.model.EmailNotification;
import com.sun.jbi.workflow.model.Message;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Subject;
import com.sun.jbi.workflow.model.Task;

public class NotificationHandler implements Handler, TaskStateListener {

    private static final Logger LOGGER = Logger
            .getLogger(NotificationHandler.class.getName());

    private String mNotificationName;

    private RuntimeTask.TaskState mNewState;

    private TaskHandlerManager mHandlerManager;

    private TaskManager mTaskManager;

    private QName mTaskQName;
    
    private static final String FROM_PART = "from";
    
    private Object mModelRef = null;

    public NotificationHandler(Notification notification,
            RuntimeTask.TaskState newState, TaskHandlerManager handlerManager,
            TaskManager taskManager, Object modelRef) {
        this.mNotificationName = notification.getName();
        this.mNewState = newState;
        this.mHandlerManager = handlerManager;
        this.mTaskManager = taskManager;
        Task taskDef = notification.getTask();

        this.mTaskQName = taskDef.getQName();
        mModelRef = modelRef;
    }

    public void execute() throws TaskException {
        this.mTaskManager.addTaskStateListener(this);
    }

    public void onStateChange(TaskStateEvent evt) {
        RuntimeTask.TaskState nState = evt.getNewState();
        if (nState == this.mNewState )  {
            if (mModelRef == null || mModelRef == evt.getModelRef () ) {
            LOGGER.log(Level.INFO, I18n.loc(
                    "WLM-5027: Sending Notification on status: {0}", nState));
            // this.mTaskManager.removeTaskStateListener(this);
            EmailNotification notification = (EmailNotification) evt.getTask()
                    .getTaskMeta().getNotification(mNotificationName);
            Subject subject = notification.getSubject();
            Message message = notification.getMessage();
            // message must be there , subject is optional
            if (message != null) {
                List<Address> addrs = notification.getAddresses();

                try {
                    processAddresses(notification, addrs, subject, message,
                            notification, evt.getTask());
                } catch (Exception ex) {
                    LOGGER
                            .log(
                                    Level.WARNING,
                                    I18n
                                            .loc("WLM-6100: Error processing Notification"),
                                    ex);
                }

            }
            }
        }
    }

    private void processAddresses(EmailNotification recipient,
            List<Address> addrs, Subject subject, Message message,
            EmailNotification notification, RuntimeTask task) throws Exception {

        Operation operation = notification.getWSDLOperation();
        String toPart = recipient.getPart();
        if (toPart == null) {
            return;
        }

        Iterator<Address> it = addrs.iterator();
        String addresses = "";
        boolean first = true;
        while (it.hasNext()) {

            Address addr = it.next();
            String address = addr.getContent(task.getJXpathContext());
            if (!first) {
                addresses = addresses + "," + address;
            } else {
                first = false;
                addresses = address;
            }
        }
        LOGGER.log(Level.INFO, I18n.loc(
                "WLM-5028: Sending Notification to : {0}", addresses));
        Map<String, NodeList> partsMap = new HashMap<String, NodeList>();

        // to part
        Document toDoc = XmlUtil.createDocument(true);
        Node addressValueNode = toDoc.createTextNode(addresses);

        partsMap.put(toPart, XmlUtil.newSingleNodeList(addressValueNode));

        // subject part
        if (subject != null) {
            String subjectPart = subject.getPart();
            if (subjectPart != null) {
                Document subjectDoc = XmlUtil.createDocument(true);
                // TODO evaluate subject xpath expression
                String subjectText = subject
                        .getContent(task.getJXpathContext());

                Node subjectValueNode = subjectDoc.createTextNode(subjectText);
                LOGGER.log(Level.INFO, I18n.loc(
                        "WLM-5029: Sending Notification subject : {0}",
                        subjectText));
                partsMap.put(subjectPart, XmlUtil
                        .newSingleNodeList(subjectValueNode));
            }
        }

        // message part
        String messagePart = message.getPart();
        if (messagePart != null) {
            Document messageDoc = XmlUtil.createDocument(true);
            // TODO evaluate message xpath expression
            Node messageValueNode = messageDoc.createTextNode(message
                    .getContent(task.getJXpathContext()));

            partsMap.put(messagePart, XmlUtil
                    .newSingleNodeList(messageValueNode));
            
        }
        
        // from part
        String fromAddress = Util.makeFromAddress();
        Document fromDoc = XmlUtil.createDocument(true);
        Node fromNode = fromDoc.createTextNode(
                fromAddress);
        LOGGER.log(Level.INFO, I18n.loc(
                "WLM-5031: Sending Notification from : {0}",
                fromAddress));
        partsMap.put(FROM_PART, XmlUtil
                .newSingleNodeList(fromNode));

        Element reply = JBIMessageUtil.makeJBIMessageWithNodeList(partsMap,
                operation);
        Task taskMeta = notification.getTask();

        PortType portType = notification.getPortType();
        QName portTypeQName = null;

        if (portType != null) {
            portTypeQName = portType.getQName();
        }
        if (portTypeQName != null) {
            mHandlerManager.notifyOnNotify(reply, portTypeQName, operation
                    .getName());
        } else {
            LOGGER
                    .log(
                            Level.WARNING,
                            I18n
                                    .loc(
                                            "WLM-6101: Can not process notification  portTypeQName is: {0}",
                                            portTypeQName));
        }

    }

    public QName getTaskDefName() {
        // TODO Auto-generated method stub
        return mTaskQName;
    }

}
