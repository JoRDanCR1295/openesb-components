/*
 * Copyright (c) 2007 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * This program, and all the routines referenced herein,
 * are the proprietary properties and trade secrets of
 * Sun Microsystems.
 *
 * Except as provided for by license agreement, this
 * program shall not be duplicated, used, or disclosed
 * without  written consent signed by an officer of
 * Sun Microsystems.
 */
package com.sun.caps.management.common.jbi;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.caps.management.common.DOMUtil;
import com.sun.caps.management.common.Util;

/**
 * This class represents the management message object that will be constructed
 * from the xml string returned by the management apis to access various parts of
 * the message. The factory method create is used to construct the dom for the xml
 * text and parse the dom to construct this object.
 */
public class JBIManagementMessage {
    /**
     * failed status
     */
    public static final String  FAILED    = "FAILED";
    
    /**
     * success status
     */
    public static final String  SUCCESS   = "SUCCESS";
    
    /**
     * info message type
     */
    public static final String  INFO      = "INFO";
    
    /**
     * warning message type
     */
    public static final String  WARNING   = "WARNING";
    
    /**
     * error message type
     */
    public static final String  ERROR     = "ERROR";
    
    /**
     * error message type
     */
    public static final String  EXCEPTION = "EXCEPTION";
    
    /**
     * unknown value for any unknown value of any msg status or type.
     */
    public static final String  UNKNOWN   = "UNKNOWN";
    
    /**
     * result object.
     */
    private FrameworkTaskResult mFrameworkTaskResult;
    
    /**
     * list.
     */
    private List<ComponentTaskResult> mCompTaskResultList;
    
    /** Creates a new instance of JBIManagementMessage */
    protected JBIManagementMessage() {
        initMessageData();
    }
    
    /**
     * initializes fields
     */
    private void initMessageData() {
        this.mFrameworkTaskResult = null;
        this.mCompTaskResultList = new ArrayList<ComponentTaskResult>();
    }
    
    /**
     * sets fw result.
     * @param frameworkTaskResult fw result object.
     */
    protected void setFrameworkTaskResult(
            FrameworkTaskResult frameworkTaskResult) {
        this.mFrameworkTaskResult = frameworkTaskResult;
    }
    
    /**
     * get fw result object.
     * @return fw result object.
     */
    protected FrameworkTaskResult getFrameworkTaskResult() {
        return this.mFrameworkTaskResult;
    }
    
    /**
     * returns result list.
     * @return result list.
     */
    protected List<ComponentTaskResult> getComponentTaskResultList() {
        return this.mCompTaskResultList;
    }
    
    /**
     * returns result list.
     * @return result list.
     */
    @SuppressWarnings("unchecked")
    protected List<ComponentTaskResult> getSortedComponentTaskResultList() {
        try {
            Collections.sort(this.mCompTaskResultList, new Comparator() {
                public int compare(Object o1, Object o2) {
                    return ((ComponentTaskResult) o1)
                            .getComponentId()
                            .compareTo(
                                    ((ComponentTaskResult) o2).getComponentId());
                }
            });
        } catch (ClassCastException ccEx) {
            // log and // do nothing.
        } catch (UnsupportedOperationException unsupEx) {
            // log and // do nothing.
        } catch (Exception ex) {
            // log and // do nothing.
        }
        
        return this.mCompTaskResultList;
    }
    
    /**
     * check message status
     * @return true if status is SUCCESS else fasle
     */
    public boolean isSuccessMsg() {
        String result = this.mFrameworkTaskResult.getResult();
        return SUCCESS.equalsIgnoreCase(result);
    }
    
    /**
     * check message status
     * @return true if status is FAILED else fasle
     */
    public boolean isFailedMsg() {
        // if not success, it is failed message
        return !isSuccessMsg();
    }
    
    /**
     * check message status
     * @return true if it is warning message else fasle
     */
    public boolean isWarningMsg() {
        return (isSuccessMsg() && isWarningMsgType());
    }
    
    /**
     * check message type
     * @return msg type or null if no message type present
     */
    public String getMessageType() {
        String msgType = this.mFrameworkTaskResult.getTaskResultInfo()
                .getMessageType();
        return msgType;
    }
    
    /**
     * check message type
     * @return true if it is warning message type else fasle
     */
    public boolean isWarningMsgType() {
        return WARNING.equalsIgnoreCase(getMessageType());
    }
    
    /**
     * check message type
     * @return true if it is info message type else fasle
     */
    public boolean isInfoMsgType() {
        return INFO.equalsIgnoreCase(getMessageType());
    }
    
    /**
     * check message type
     * @return true if it is error message type else fasle
     */
    public boolean isErrorMsgType() {
        return ERROR.equalsIgnoreCase(getMessageType());
    }
    
    /**
     * check message type
     * @return true if it is error message type else fasle
     */
    public boolean isExceptionMsgType() {
        String type = getMessageType();
        if (type == null || UNKNOWN.equalsIgnoreCase(type)
                || EXCEPTION.equalsIgnoreCase(type)) {
            return this.getFrameworkTaskResult().isException();
        } else {
            return false;
        }
    }
    
    /**
     * override method
     * @param s stream
     */
    public void printMessage(PrintStream s) {
        PrintWriter writer = new PrintWriter(s);
        printMessage(writer);
    }
    
    /**
     * Returns a verbose output.
     * @return debug message.
     */
    public String getDebugMessage() {
        StringWriter msgBuffer = new StringWriter();
        PrintWriter msgWriter = new PrintWriter(msgBuffer);
        
        msgWriter.println(Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.msg.debug.fw.msg.header"));
        
        String formattedFrameworkMsg = Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.msg.debug.fw.msg",
                new Object[] { this.mFrameworkTaskResult.getTaskId(),
                        this.mFrameworkTaskResult.getResult(), // TODO i18n
                        this.mFrameworkTaskResult.getMessage() });
        
        msgWriter.println(formattedFrameworkMsg);
        
        msgWriter.println(Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.msg.debug.comp.msg.header"));
        
        List<ComponentTaskResult> compTaskResultList = this.getComponentTaskResultList();
        for (int i = 0; i < compTaskResultList.size(); ++i) {
            ComponentTaskResult compTaskResult = (ComponentTaskResult) compTaskResultList
                    .get(i);
            String compName = compTaskResult.getComponentId();
            
            // print component name
            String compMsgTitle = Util.getCommonI18NBundle().getMessage(
                    "jbi.mgmt.msg.debug.for.comp", compName);
            msgWriter.println(compMsgTitle);
            // print component messages
            String formattedCompMsg = Util.getCommonI18NBundle().getMessage(
                    "jbi.mgmt.msg.debug.comp.msg",
                    new Object[] { compTaskResult.getTaskId(),
                            compTaskResult.getResult(), // todo i18n
                            compTaskResult.getMessage() });
            msgWriter.println(formattedCompMsg);
        }
        
        closeWriter(msgWriter);
        closeWriter(msgBuffer);
        return msgBuffer.getBuffer().toString();
    }
    
    /**
     * prints message
     * @param writer writer
     */
    public void printDebugMessage(PrintWriter writer) {
        String outMsg = getDebugMessage();
        writer.println(outMsg);
    }
    
    /**
     * closes the writer without generating the IOException
     * @param writer Writer object
     */
    protected void closeWriter(Writer writer) {
        try {
            writer.close();
        } catch (IOException ex) {
            // ignore
        }
    }
    
    /**
     * 
     * @param out 
     * @param msgList 
     */
    protected void printMessageList(PrintWriter out, List<String> msgList) {
        String msg = (String) msgList.get(0);
        out.print(msg);
        for (int i = 1; i < msgList.size(); ++i) {
            out.println();
            msg = (String) msgList.get(i);
            out.print(msg);
        }
    }
    
    /**
     * 
     * @param compTaskResult 
     * @return 
     */
    protected List<String> getAllMessagesForComponent(ComponentTaskResult compTaskResult) {
        boolean compFormat = true;
        if (compTaskResult == null) {
            return new ArrayList<String>();
        }
        return compTaskResult.getTaskResultInfo().getAllFormattedMessages(
                compFormat);
    }
    
    /**
     * returns the formatted message as a string
     * @param level Specifies the level of output ("verbose","normal","terse")
     * @return message.
     * @deprecated
     */
    public String getMessageDeprecated(String level) {
        // Make sure the level is a valid value (Defauld to "verbose")
        if ((!(level.equalsIgnoreCase("normal")))
                && (!(level.equalsIgnoreCase("terse")))) {
            level = "verbose";
        }
        
        StringWriter msgBuffer = new StringWriter();
        PrintWriter msgWriter = new PrintWriter(msgBuffer);
        
        StringWriter fwMsgBuffer = new StringWriter();
        PrintWriter fwMsgWriter = new PrintWriter(fwMsgBuffer);
        
        StringWriter compMsgBuffer = new StringWriter();
        PrintWriter compMsgWriter = new PrintWriter(compMsgBuffer);
        
        boolean containsFwMsgs = false;
        boolean containsCompMsgs = false;
        
        boolean compFormat = true;
        
        // retrieve fw messages
        List<String> fwMsgList = this.getFrameworkTaskResult().getTaskResultInfo()
                .getAllFormattedMessages(!compFormat);
        // print fw messages
        if (fwMsgList != null && fwMsgList.size() > 0) {
            printMessageList(fwMsgWriter, fwMsgList);
            containsFwMsgs = true;
        }
        
        boolean firstCompMsgPrinted = false;
        
        // retrieve comp messages
        
        List<ComponentTaskResult> compTaskResultList = this.getSortedComponentTaskResultList();
        
        for (int i = 0; i < compTaskResultList.size(); ++i) {
            ComponentTaskResult compTaskResult = (ComponentTaskResult) compTaskResultList
                    .get(i);
            String compName = compTaskResult.getComponentId();
            List<String> compMsgList = getAllMessagesForComponent(compTaskResult);
            if (compMsgList != null && compMsgList.size() > 0) {
                if (firstCompMsgPrinted) {
                    compMsgWriter.println();
                }
                // print component name
                String compMsgTitle = Util.getCommonI18NBundle().getMessage(
                        "jbi.result.comp.msg.from.comp", compName);
                compMsgWriter.println(compMsgTitle);
                // print component messages
                printMessageList(compMsgWriter, compMsgList);
                firstCompMsgPrinted = true;
                containsCompMsgs = true;
            }
        }
        
        closeWriter(fwMsgWriter);
        closeWriter(compMsgWriter);
        closeWriter(fwMsgBuffer);
        closeWriter(compMsgBuffer);
        
        // print fw msgs
        if (containsFwMsgs) {
            msgWriter.print(fwMsgBuffer.getBuffer().toString());
        }
        
        // print comp msgs
        if (containsCompMsgs) {
            if (containsFwMsgs) {
                msgWriter.println();
            }
            msgWriter.print(compMsgBuffer.getBuffer().toString());
        }
        
        closeWriter(msgWriter);
        closeWriter(msgBuffer);
        
        return msgBuffer.getBuffer().toString();
    }
    
    /**
     * returns the formatted message as a string
     * @param level Specifies the level of output ("verbose","normal","terse")
     * @return message.
     */
    public String getMessage(String level) {
        // Make sure the level is a valid value (Default to "verbose")
        if ((!(level.equalsIgnoreCase("normal")))
                && (!(level.equalsIgnoreCase("terse")))) {
            level = "verbose";
        }
        
        // ESBResultFormatter
        ESBResultFormatter formatter = new ESBResultFormatter(level);
        return formatter.getFormattedESBResult(this);
    }
    
    /**
     * returns the formatted message as a string
     * @return text
     */
    public String getMessage() {
        return (getMessage("verbose"));
    }
    
    /**
     * prints message
     * @param writer writer
     */
    public void printMessage(PrintWriter writer) {
        String outMsg = getMessage();
        writer.println(outMsg);
    }
    
    /**
     * override method
     * @param s stream
     */
    public void printStackTrace(PrintStream s) {
        PrintWriter writer = new PrintWriter(s, true);
        printStackTrace(writer);
    }
    
    /**
     * write string to the writer
     * @param prefix string
     * @param msg string
     * @param writer Print Writer
     */
    public static void printWithIndentation(String prefix, String msg,
            PrintWriter writer) {
        if (msg == null) {
            return;
        }
        
        BufferedReader reader = new BufferedReader(new StringReader(msg));
        String line = null;
        try {
            while ((line = reader.readLine()) != null) {
                writer.println(prefix + line);
            }
        } catch (IOException ex) {
            // will never happen. ignore
            
        }
    }
    
    /**
     * prints stacktrace
     * @param writer writer
     */
    public void printStackTrace(PrintWriter writer) {
        String fwSTPrefix = Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.stack.fw.stack.prefix");
        
        String compSTPrefix = Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.stack.comp.stack.prefix");
        
        writer.println(Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.stack.fw.stack.header"));
        writer.print(this.mFrameworkTaskResult.getMessage());
        String frameworkStack = this.mFrameworkTaskResult.getStackTrace();
        printWithIndentation(fwSTPrefix, frameworkStack, writer);
        
        writer.println(Util.getCommonI18NBundle().getMessage(
                "jbi.mgmt.stack.comp.stack.header"));
        
        List<ComponentTaskResult> compTaskResultList = this.getComponentTaskResultList();
        for (int i = 0; i < compTaskResultList.size(); ++i) {
            ComponentTaskResult compTaskResult = (ComponentTaskResult) compTaskResultList
                    .get(i);
            String compName = compTaskResult.getComponentId();
            
            // print component name
            String compMsgTitle = Util.getCommonI18NBundle().getMessage(
                    "jbi.mgmt.stack.for.comp", compName);
            writer.println(compMsgTitle);
            // print component stack
            String compStack = compTaskResult.getStackTrace();
            writer.print(compTaskResult.getMessage());
            printWithIndentation(compSTPrefix, compStack, writer);
        }
        
    }
    
    /**
     * stacktrace
     * @return text
     */
    public String getStackTrace() {
        StringWriter strWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(strWriter, true);
        printStackTrace(writer);
        writer.close();
        return strWriter.getBuffer().toString();
    }
    
    /**
     * adds comp result object.
     * @param compTaskResult comp result object.
     */
    private void addToComponentResultList(ComponentTaskResult compTaskResult) {
        if (compTaskResult == null) {
            return;
        }
        
        this.mCompTaskResultList.add(compTaskResult);
        
    }
    
    /**
     * factory method to create the message object
     * @param xmlText xml text
     * @return Message Object
     */
    public static JBIManagementMessage createJBIManagementMessage(String xmlText) {
        JBIManagementMessageUnmarshaller unmarshaller = new JBIManagementMessageUnmarshaller();
        try {
            return unmarshaller.unmarshal(xmlText);
        } catch (Exception ex) {
            // don't care the exception. we either create or don't create the
            // mgmt object out of the string. code using this api will take care of why
            return null;
        }
    }
    
    /**
     * returns the l10n msgtyep string for msgType
     * @param msgType is one of the INFO, ERROR, WARNING
     * @return 
     */
    protected static String getL10NMessageType(String msgType) {
        
        String l10nMsgType = null;
        if (ERROR.equalsIgnoreCase(msgType)) {
            l10nMsgType = Util.getCommonI18NBundle().getMessage(
                    "jbi.result.msg.type.error");
        } else if (WARNING.equalsIgnoreCase(msgType)) {
            l10nMsgType = Util.getCommonI18NBundle().getMessage(
                    "jbi.result.msg.type.warning");
        } else if (INFO.equalsIgnoreCase(msgType)) {
            l10nMsgType = Util.getCommonI18NBundle().getMessage(
                    "jbi.result.msg.type.info");
        } else {
            l10nMsgType = Util.getCommonI18NBundle().getMessage(
                    "jbi.result.msg.type.unknown");
        }
        return l10nMsgType;
        
    }
    
    /**
     * class for message inf object.
     */
    public static class MessageInfo {
        /**
         * id.
         */
        private String mI18nId;
        
        /**
         * msg.
         */
        private String mLocalizedMsg;
        
        /**
         * params.
         */
        private List<String>   ml10nMsgParams;
        
        /**
         * constructor.
         * @param id id.
         * @param locMsg msg.
         * @param locMsgParams params.
         */
        @SuppressWarnings("unchecked")
        public MessageInfo(String id, String locMsg, List<String> locMsgParams) {
            this.mI18nId = id;
            this.mLocalizedMsg = locMsg;
            
            if (locMsgParams != null) {
                this.ml10nMsgParams = Collections
                        .unmodifiableList(locMsgParams);
            } else {
                this.ml10nMsgParams = new ArrayList();
            }
        }
        
        /**
         * return id.
         * @return id.
         */
        public String getI18nId() {
            return this.mI18nId;
        }
        
        /**
         * returns message.
         * @return message.
         */
        public String getLocalizedMsg() {
            return this.mLocalizedMsg;
        }
        
        /**
         * returns params.
         * @return list of params.
         */
        public List<String> getl10nMsgParams() {
            return this.ml10nMsgParams;
        }
        
        /**
         * returns formatted message.
         * @paramn isExpFormat  true if the format is as a exception msg
         * @return formatted message
         * @param isExpFormat 
         */
        public String getFormattedMessage(boolean isExpFormat) {
            //TODO define a static initilizer for the i18nMsgType
            String formattedMsg = null;
            formattedMsg = Util.getCommonI18NBundle().getMessage(
                    "jbi.result.msg.format", this.getI18nId(),
                    this.getLocalizedMsg());
            
            if (isExpFormat) {
                // put the formatted msg as expcetion msg
                formattedMsg = Util.getCommonI18NBundle().getMessage(
                        "jbi.result.ex.msg.format", formattedMsg);
            }
            
            return formattedMsg;
        }
    }
    
    /**
     * class for Exception info storage.
     */
    public static class ExceptionInfo {
        /**
         * level.
         */
        private Integer     mLevel;
        
        /**
         * msg info object.
         */
        private MessageInfo mMsgInfo;
        
        /**
         * stacktrace.
         */
        private String      mStacktrace;
        
        /**
         * constructor.
         * @param level level.
         * @param msgInfo msg info object.
         * @param stackTrace stacktrace string.
         */
        public ExceptionInfo(String level, MessageInfo msgInfo,
                String stackTrace) {
            try {
                this.mLevel = Integer.valueOf(level);
            } catch (NumberFormatException ex) {
                this.mLevel = new Integer(-1);
            }
            this.mMsgInfo = msgInfo;
            this.mStacktrace = stackTrace;
        }
        
        /**
         * returns level.
         * @return Integer level.
         */
        public Integer getLevel() {
            return this.mLevel;
        }
        
        /**
         * returns stacktrace string.
         * @return text for stacktrace.
         */
        public String getStackTrace() {
            return this.mStacktrace;
        }
        
        /**
         * returns message info object.
         * @return MessageInfo object.
         */
        public MessageInfo getMessageInfo() {
            return this.mMsgInfo;
        }
        
        /**
         * returns message.
         * @return message.
         */
        public String getLocalizedMsg() {
            if (this.mMsgInfo != null) {
                return this.mMsgInfo.getLocalizedMsg();
            } else {
                return null;
            }
        }
        
        /**
         * returns formatted message.
         * @paramn isExpFormat  true if the format is as a exception msg
         * @return formatted message
         * @param isExpFormat 
         */
        public String getFormattedMessage(boolean isExpFormat) {
            if (this.mMsgInfo != null) {
                return this.mMsgInfo.getFormattedMessage(isExpFormat);
            } else {
                return null;
            }
        }
        
        /**
         * returns formatted stacktrace.
         * @return formatted stacktrace.
         */
        public String getFormattedStackTrace() {
            //TODO add "at " prefix to every line of the stacktrace.
            return this.mStacktrace;
        }
    }
    
    /**
     * class for task result object
     */
    public static class TaskResultInfo {
        
        /**
         * TaskId.
         */
        private String mTaskId;
        
        /**
         * Result
         */
        private String mResult;
        
        /**
         * MsgType;
         */
        private String mMsgType;
        
        /**
         * MsgInfoList
         */
        private List<MessageInfo>   mMsgInfoList;
        
        /**
         * ExInfoList
         */
        private List<ExceptionInfo>   mExInfoList;
        
        /**
         * default constructor
         */
        public TaskResultInfo() {
            this.mTaskId = UNKNOWN;
            this.mResult = UNKNOWN;
            this.mMsgType = UNKNOWN;
            this.mMsgInfoList = new ArrayList<MessageInfo>();
            this.mExInfoList = new ArrayList<ExceptionInfo>();
        }
        
        /**
         * constructor
         * @param taskId id.
         * @param result result.
         * @param msgType type.
         * @param msgInfoList a List of MessageInfo.
         * @param exInfoList list.
         */
        @SuppressWarnings("unchecked")
        public TaskResultInfo(String taskId, String result, String msgType,
                List<MessageInfo> msgInfoList, List<ExceptionInfo> exInfoList) {
            this.mTaskId = taskId;
            this.mResult = result;
            this.mMsgType = msgType;
            
            if (msgInfoList != null) {
                this.mMsgInfoList = Collections.unmodifiableList(msgInfoList);
            } else {
                this.mMsgInfoList = new ArrayList<MessageInfo>();
            }
            
            if (exInfoList != null) {
                this.mExInfoList = Collections.unmodifiableList(exInfoList);
            } else {
                this.mExInfoList = new ArrayList<ExceptionInfo>();
            }
            
        }
        
        /**
         * returns id
         * @return id
         */
        public String getTaskId() {
            return this.mTaskId;
        }
        
        public void setTaskId(String taskId) {
            this.mTaskId = taskId;
        }
        
        /**
         * returns result
         * @return result
         */
        public String getResult() {
            return this.mResult;
        }
        
        public void setResult(String result) {
            this.mResult = result;
        }
        
        /**
         * returns message type
         * @return type
         */
        public String getMessageType() {
            return this.mMsgType;
        }
        
        public void setMessageType(String msgType) {
            this.mMsgType = msgType;
        }
        
        /**
         * returns message info list
         * @return message info list
         */
        public List<MessageInfo> getStatusMessageInfoList() {
            return this.mMsgInfoList;
        }
        
        public void setStatusMessageInfoList(List<MessageInfo> msgInfoList) {
            this.mMsgInfoList = msgInfoList;
        }
        
        /**
         * returns exception info list
         * @return exception info list
         */
        public List<ExceptionInfo> getExceptionInfoList() {
            return this.mExInfoList;
        }
        
        public void setExceptionInfoList(List<ExceptionInfo> expInfoList) {
            this.mExInfoList = expInfoList;
        }
        
        /**
         * return status message
         * @return status message
         */
        public String getStatusMessage() {
            if (this.mMsgInfoList == null || this.mMsgInfoList.size() <= 0) {
                return null;
            }
            StringWriter strWriter = new StringWriter();
            PrintWriter prnWriter = new PrintWriter(strWriter);
            for (Iterator<MessageInfo> itr = this.mMsgInfoList.iterator(); itr.hasNext();) {
                MessageInfo msgInfo = (MessageInfo) itr.next();
                String msg = msgInfo.getFormattedMessage(false);
                prnWriter.println(msg);
            }
            
            try {
                prnWriter.close();
                strWriter.close();
            } catch (IOException ex) {
                // ignore
            }
            return strWriter.getBuffer().toString();
        }
        
        /**
         * returns exception message
         * @return exception messgae
         */
        public String getExceptionMessage() {
            if (this.mExInfoList == null || this.mExInfoList.size() <= 0) {
                return null;
            }
            StringWriter strWriter = new StringWriter();
            PrintWriter prnWriter = new PrintWriter(strWriter);
            for (Iterator<ExceptionInfo> itr = this.mExInfoList.iterator(); itr.hasNext();) {
                ExceptionInfo exInfo = (ExceptionInfo) itr.next();
                String exMsg = exInfo.getFormattedMessage(true);
                // TODO format the chained exception message here
                prnWriter.println(exMsg);
            }
            
            try {
                prnWriter.close();
                strWriter.close();
            } catch (IOException ex) {
                // ignore
            }
            return strWriter.getBuffer().toString();
        }
        
        /**
         * returns the status msg if present else returns the exception message
         * @return message.
         */
        public String getMessage() {
            String msg = this.getStatusMessage();
            if (msg != null) {
                return msg;
            }
            msg = this.getExceptionMessage();
            return msg;
        }
        
        /**
         * check if this message is from exception
         * @return true if it is exception else false.
         */
        public boolean isException() {
            return (this.mExInfoList != null && this.mExInfoList.size() > 0);
        }
        
        /**
         * return stacktrace
         * @return stacktrace
         */
        public String getStackTrace() {
            if (this.mExInfoList == null || this.mExInfoList.size() <= 0) {
                return null;
            }
            StringWriter strWriter = new StringWriter();
            PrintWriter prnWriter = new PrintWriter(strWriter);
            for (Iterator<ExceptionInfo> itr = this.mExInfoList.iterator(); itr.hasNext();) {
                ExceptionInfo exInfo = (ExceptionInfo) itr.next();
                String stacktrace = exInfo.getStackTrace();
                prnWriter.println(stacktrace);
            }
            
            try {
                prnWriter.close();
                strWriter.close();
            } catch (IOException ex) {
                // ignore
            }
            return strWriter.getBuffer().toString();
        }
        
        /**
         * returns the msg formatted as <msgtype>:(<msgcode>)<msg>
         * @return formatted 1st message
         * @param formattedMsg 
         * @param msgType message type from one of ERROR, WARNING, INFO
         */
        protected String getFormattedMsgWithType(String msgType,
                String formattedMsg) {
            String l10nType = getL10NMessageType(msgType);
            return Util.getCommonI18NBundle().getMessage(
                    "jbi.result.1st.msg.format", l10nType, formattedMsg);
        }
        
        /**
         * formats the indentation of the formattedMsg as framework or component message indentation
         * @param isCompFormat true for component indentation, false for framework indentation
         * @param formattedMsg message that is formatted with low level format.
         * @return 
         */
        protected String getFormatted1stMessage(boolean isCompFormat,
                String formattedMsg) {
            String i18nKey = "jbi.result.fw.msg";
            if (isCompFormat) {
                i18nKey = "jbi.result.comp.msg";
            }
            return Util.getCommonI18NBundle().getMessage(i18nKey, formattedMsg);
        }
        
        /**
         * formats the indentation of the formattedMsg as framework or component message indentation
         * @param isCompFormat true for component indentation, false for framework indentation
         * @param formattedMsg message that is formatted with low level format.
         * @return 
         */
        protected String getFormatted2ndMessage(boolean isCompFormat,
                String formattedMsg) {
            String i18nKey = "jbi.result.fw.2nd.msg";
            if (isCompFormat) {
                i18nKey = "jbi.result.comp.2nd.msg";
            }
            return Util.getCommonI18NBundle().getMessage(i18nKey, formattedMsg);
        }
        
        /**
         * formats the indentation of the formattedMsg as framework or component message indentation
         * @param isCompFormat true for component indentation, false for framework indentation
         * @param formattedMsg message that is formatted with low level format.
         * @return 
         */
        protected String getFormatted1stExpMessage(boolean isCompFormat,
                String formattedMsg) {
            String i18nKey = "jbi.result.fw.ex.msg";
            if (isCompFormat) {
                i18nKey = "jbi.result.comp.ex.msg";
            }
            return Util.getCommonI18NBundle().getMessage(i18nKey, formattedMsg);
        }
        
        /**
         * formats the indentation of the formattedMsg as framework or component message indentation
         * @param isCompFormat true for component indentation, false for framework indentation
         * @param formattedMsg message that is formatted with low level format.
         * @return 
         */
        protected String getFormatted2ndExpMessage(boolean isCompFormat,
                String formattedMsg) {
            String i18nKey = "jbi.result.fw.2nd.ex.msg";
            if (isCompFormat) {
                i18nKey = "jbi.result.comp.2nd.ex.msg";
            }
            return Util.getCommonI18NBundle().getMessage(i18nKey, formattedMsg);
        }
        
        /**
         * 
         * @param msgInfoList 
         * @param msgType 
         * @param isCompFormat 
         * @return 
         */
        protected List<String> getFormattedStatusMessages(List<MessageInfo> msgInfoList,
                String msgType, boolean isCompFormat) {
            List<String> msgList = new ArrayList<String>();
            String formattedMsg = null;
            boolean expFormat = true;
            // now get the formatted messages into the list
            
            int msgInfoListSize = msgInfoList.size();
            for (int i = 0; i < msgInfoListSize; ++i) {
                MessageInfo msgInfo = (MessageInfo) msgInfoList.get(i);
                formattedMsg = null;
                if (i == 0) {
                    
                    // add as a ist msg
                    formattedMsg = getFormattedMsgWithType(msgType, msgInfo
                            .getFormattedMessage(!expFormat));
                    formattedMsg = getFormatted1stMessage(isCompFormat,
                            formattedMsg);
                    
                } else {
                    // add all other msgs as normal msgs and as 2nd msg
                    formattedMsg = msgInfo.getFormattedMessage(!expFormat);
                    formattedMsg = getFormatted2ndMessage(isCompFormat,
                            formattedMsg);
                }
                msgList.add(formattedMsg);
            }
            return msgList;
        }
        
        /**
         * 
         * @param exList 
         * @param msgType 
         * @param isCompFormat 
         * @param printExAsMainMsg 
         * @return 
         */
        protected List<String> getFormattedExceptionMessages(List<ExceptionInfo> exList,
                String msgType, boolean isCompFormat, boolean printExAsMainMsg) {
            
            List<String> msgList = new ArrayList<String>();
            String formattedMsg = null;
            boolean expFormat = true;
            // now get the exception messages into the list
            boolean exAsMainMsgPrinted = false;
            int expListSize = exList.size();
            for (int i = 0; i < expListSize; ++i) {
                ExceptionInfo exInfo = (ExceptionInfo) exList.get(i);
                formattedMsg = null;
                if (i == 0) {
                    if (printExAsMainMsg) {
                        // no status messages. so add first except as 1st main message
                        formattedMsg = getFormattedMsgWithType(msgType, exInfo
                                .getFormattedMessage(!expFormat));
                        // get formatted fw 1st msg
                        formattedMsg = getFormatted1stMessage(isCompFormat,
                                formattedMsg);
                        exAsMainMsgPrinted = true;
                    } else {
                        // add the first exception as exp message
                        formattedMsg = exInfo.getFormattedMessage(expFormat);
                        formattedMsg = getFormatted1stExpMessage(isCompFormat,
                                formattedMsg);
                        
                    }
                } else if (exAsMainMsgPrinted && i == 1) {
                    // print the 2nd exception message as exception message
                    formattedMsg = exInfo.getFormattedMessage(expFormat);
                    formattedMsg = getFormatted1stExpMessage(isCompFormat,
                            formattedMsg);
                } else {
                    // add all exceptions as normal msgs and as 2nd msg
                    formattedMsg = exInfo.getFormattedMessage(!expFormat);
                    formattedMsg = getFormatted2ndExpMessage(isCompFormat,
                            formattedMsg);
                }
                msgList.add(formattedMsg);
            }
            return msgList;
        }
        
        /**
         * return formatted messages.
         * @return list.
         * @param isCompFormat 
         */
        public List<String> getAllFormattedMessages(boolean isCompFormat) {
            List<String> fmtMsgList = new ArrayList<String>();
            
            String msgType = this.getMessageType();
            List<MessageInfo> msgInfoList = this.getStatusMessageInfoList();
            List<ExceptionInfo> exInfoList = this.getExceptionInfoList();
            
            boolean printExAsMainMsg = false;
            
            // get the status messages into the list
            
            List<String> fmtStatusMsgList = getFormattedStatusMessages(msgInfoList,
                    msgType, isCompFormat);
            
            if (fmtStatusMsgList.size() == 0) {
                printExAsMainMsg = true;
            }
            
            List<String> fmtExMsgList = this.getFormattedExceptionMessages(exInfoList,
                    msgType, isCompFormat, printExAsMainMsg);
            
            fmtMsgList.addAll(fmtStatusMsgList);
            fmtMsgList.addAll(fmtExMsgList);
            
            return fmtMsgList;
            
        }
        
        /**
         * return formatted stacktraces.
         * @return list.
         */
        public List<String> getFormattedStacktraces() {
            List<String> list = new ArrayList<String>();
            List<ExceptionInfo> exList = this.getExceptionInfoList();
            
            for (Iterator<ExceptionInfo> itr = exList.iterator(); itr.hasNext();) {
                ExceptionInfo exInfo = (ExceptionInfo) itr.next();
                list.add(exInfo.getFormattedStackTrace());
            }
            return list;
        }
        
    }
    
    /**
     * base class for task result object.
     */
    public static abstract class AbstractTaskResult {
        /**
         * TaskResultInfo object.
         */
        private TaskResultInfo mTaskResultInfo;
        
        protected AbstractTaskResult() {
            this.mTaskResultInfo = new TaskResultInfo();
        }
        
        /**
         * return TaskResultInfo
         * @return TaskResultInfo
         */
        public TaskResultInfo getTaskResultInfo() {
            return this.mTaskResultInfo;
        }
        
        public void setTaskResultInfo(TaskResultInfo taskResultInfo) {
            this.mTaskResultInfo = taskResultInfo;
        }
        
        /**
         * TaskId
         * @return TaskId
         */
        public String getTaskId() {
            return this.getTaskResultInfo().getTaskId();
        }
        
        /**
         * Result
         * @return Result
         */
        public String getResult() {
            return this.getTaskResultInfo().getResult();
        }
        
        /**
         * Message
         * @return Message
         */
        public String getMessage() {
            return this.getTaskResultInfo().getMessage();
        }
        
        /**
         * Exception
         * @return true if exception else false
         */
        public boolean isException() {
            return this.getTaskResultInfo().isException();
        }
        
        /**
         * StackTrace.
         * @return StackTrace
         */
        public String getStackTrace() {
            return this.getTaskResultInfo().getStackTrace();
        }
        
    }
    
    /**
     * class for Framework task results object
     */
    public static class FrameworkTaskResult extends AbstractTaskResult {
        /**
         * IsCauseFramework
         */
        private boolean mIsCauseFramework;
        
        /**
         * Locale
         */
        private String  mLocale;
        
        public FrameworkTaskResult() {
            super();
            this.mIsCauseFramework = false;
            this.mLocale = Locale.getDefault().toString();
        }
        
        /**
         * constructor.
         * @param taskResultInfo object.
         * @param isCauseFramework cause value.
         * @param locale locale.
         */
        public FrameworkTaskResult(TaskResultInfo taskResultInfo,
                boolean isCauseFramework, String locale) {
            super();
            this.setTaskResultInfo(taskResultInfo);
            this.mIsCauseFramework = isCauseFramework;
            this.mLocale = locale;
        }
        
        /**
         * return Locale
         * @return Locale
         */
        public String getLocale() {
            return this.mLocale;
        }
        
        public void setLocale(String locale) {
            this.mLocale = locale;
        }
        
        public void setIsCauseFramework(boolean isCauseFramework) {
            this.mIsCauseFramework = isCauseFramework;
        }
        
        /**
         * return true if cause is framework else false
         * @return true if cause is framework else false
         */
        public boolean isCauseFramework() {
            return this.mIsCauseFramework;
        }
        
        /**
         * return true if cause is component else false
         * @return true if cause is component else false
         */
        public boolean isCauseComponent() {
            return !isCauseFramework();
        }
        
    }
    
    /**
     * class for component result
     */
    public static class ComponentTaskResult extends AbstractTaskResult {
        /**
         * ComponentId
         */
        private String mComponentId;
        
        public ComponentTaskResult() {
            super();
            this.mComponentId = null;
        }
        
        /**
         * constructor
         * @param taskResultInfo object.
         * @param aComponentId component id.
         */
        public ComponentTaskResult(TaskResultInfo taskResultInfo,
                String aComponentId) {
            super();
            this.setTaskResultInfo(taskResultInfo);
            this.mComponentId = aComponentId;
        }
        
        /**
         * return ComponentId
         * @return ComponentId
         */
        public String getComponentId() {
            return this.mComponentId;
        }
        
        /**
         * return ComponentId
         * @return ComponentId
         */
        public String getComponentName() {
            return getComponentId();
        }
        
    }
    
    /**
     * class for jbi task resutls that consists of {FrameworkTaskResult, ComponentTaskResult*}
     */
    public static class JBITaskResult {
        /**
         * result object.
         */
        private FrameworkTaskResult mFrameworkTaskResult;
        
        /**
         * list.
         */
        private List<ComponentTaskResult> mCompTaskResultList;
        
        public JBITaskResult() {
            this.mFrameworkTaskResult = new FrameworkTaskResult();
            this.mCompTaskResultList = new ArrayList<ComponentTaskResult>();
        }
        
        public JBITaskResult(FrameworkTaskResult fwTaskResult,
                List<ComponentTaskResult> compTaskResultList) {
            this.mFrameworkTaskResult = fwTaskResult;
            this.mCompTaskResultList = compTaskResultList;
        }
        
        public FrameworkTaskResult getFrameworkTaskResult() {
            return this.mFrameworkTaskResult;
        }
        
        public void setFrameworkTaskResult(FrameworkTaskResult fwTaskResult) {
            this.mFrameworkTaskResult = fwTaskResult;
        }
        
        public List<ComponentTaskResult> getComponentTaskResultList() {
            return this.mCompTaskResultList;
        }
        
        public void setComponentTaskResultList(
                List<ComponentTaskResult> compTaskResultList) {
            this.mCompTaskResultList = compTaskResultList;
        }
        
    }
    
    /**
     * class for instance result in ESB 
     */
    public static class InstanceTaskResult {
        /**
         * name of the instance in the ESB
         */
        private String        mInstanceName;
        
        /**
         * result object.
         */
        private JBITaskResult mJBITaskResult;
        
        public InstanceTaskResult() {
            this.mInstanceName = null;
            this.mJBITaskResult = new JBITaskResult();
        }
        
        public InstanceTaskResult(String instanceName,
                JBITaskResult jbiTaskResult) {
            this.mInstanceName = instanceName;
            this.mJBITaskResult = jbiTaskResult;
        }
        
        public String getInstanceName() {
            return this.mInstanceName;
        }
        
        public void setInstanceName(String instanceName) {
            this.mInstanceName = instanceName;
        }
        
        public JBITaskResult getJBITaskResult() {
            return this.mJBITaskResult;
        }
        
        public void setJBITaskResult(JBITaskResult jbiTaskResult) {
            this.mJBITaskResult = jbiTaskResult;
        }
        
    }
    
    /**
     * class for esb task resutls that consists of {JBITaskResult, ESBInstanceTaskResult*}
     */
    public static class ESBTaskResult {
        /**
         * result object.
         */
        private JBITaskResult mCASTaskResult;
        
        /**
         * list.
         */
        private List<InstanceTaskResult> mInstTaskResultList;
        
        public ESBTaskResult() {
            this.mCASTaskResult = new JBITaskResult();
            this.mInstTaskResultList = new ArrayList<InstanceTaskResult>();
        }
        
        public ESBTaskResult(JBITaskResult casTaskResult,
                List<InstanceTaskResult> instTaskResultList) {
            this.mCASTaskResult = casTaskResult;
            this.mInstTaskResultList = instTaskResultList;
        }
        
        public JBITaskResult getCASTaskResult() {
            return this.mCASTaskResult;
        }
        
        public void setCASTaskResult(JBITaskResult casTaskResult) {
            this.mCASTaskResult = casTaskResult;
        }
        
        public List<InstanceTaskResult> getInstanceTaskResultList() {
            return this.mInstTaskResultList;
        }
        
        public void setInstanceTaskResultList(
                List<InstanceTaskResult> instTaskResultList) {
            this.mInstTaskResultList = instTaskResultList;
        }
    }
    
    /**
     * to convert the management message xml to the JBIManagementMessageObject.
     */
    public static class JBIManagementMessageUnmarshaller {
        /**
         * factory method to create the message object
         * @param xmlText xml text
         * @return Message Object
         */
        public JBIManagementMessage unmarshal(String xmlText) {
            
            Document xmlDoc = null;
            
            try {
                DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory
                        .newInstance();
                
                DocumentBuilder docBuilder = docBuilderFactory
                        .newDocumentBuilder();
                docBuilder.setErrorHandler(new DefaultHandler() {
                    public void fatalError(SAXParseException e)
                            throws SAXException {
                        throw new SAXException(e.getMessage());
                    }
                });
                
                StringReader reader = new StringReader(xmlText);
                InputSource is = new InputSource(reader);
                xmlDoc = docBuilder.parse(is);
            } catch (Exception ex) {
                // any error means the message not understood.
                return null;
            }
            
            if (xmlDoc == null) {
                return null;
            }
            
            JBIManagementMessage msg = new JBIManagementMessage();
            
            // check if the xml contains the jbi mgmt msg root elements
            
            Element rootEl = DOMUtil.UTIL.getElement(xmlDoc, "jbi-task");
            
            if (rootEl == null) {
                return null;
            }
            
            Element taskResultInfo = DOMUtil.UTIL.getElement(xmlDoc,
                    "jbi-task-result");
            
            if (taskResultInfo == null) {
                return null;
            }
            
            Element fwInfoEl = DOMUtil.UTIL.getElement(taskResultInfo,
                    "frmwk-task-result");
            
            if (fwInfoEl == null) {
                return null;
            }
            
            FrameworkTaskResult fwTaskResult = getFrameworkTaskResult(fwInfoEl);
            msg.setFrameworkTaskResult(fwTaskResult);
            
            NodeList compInfoList = DOMUtil.UTIL.getChildElements(
                    taskResultInfo, "component-task-result");
            if (compInfoList != null) {
                int size = compInfoList.getLength();
                for (int i = 0; i < size; ++i) {
                    Element compInfoElement = (Element) compInfoList.item(i);
                    if (compInfoElement != null) {
                        ComponentTaskResult compTaskResult = getComponentTaskResult(compInfoElement);
                        msg.addToComponentResultList(compTaskResult);
                    }
                }
            }
            return msg;
            
        }
        
        /**
         * return FrameworkLocale
         * @param fwInfoEl xml element
         * @return FrameworkLocale
         */
        private String getFrameworkLocale(Element fwInfoEl) {
            String locale = null; // todo defaul en?
            
            Element localeEl = DOMUtil.UTIL.getElement(fwInfoEl, "locale");
            if (localeEl != null) {
                locale = DOMUtil.UTIL.getTextData(localeEl);
            }
            
            return locale;
        }
        
        /**
         * return true if cause if component else false
         * @param fwInfoEl xml element
         * @return true if cause if component else false
         */
        private boolean isCauseComponent(Element fwInfoEl) {
            String value = "NO"; // true, false?
            
            Element isCauseComponentEl = DOMUtil.UTIL.getElement(fwInfoEl,
                    "is-cause-component");
            if (isCauseComponentEl != null) {
                value = DOMUtil.UTIL.getTextData(isCauseComponentEl);
            } else {
                value = "NO";
            }
            // anything other than yes. is no.
            return value.equalsIgnoreCase("YES");
        }
        
        /**
         * return true if cause if component else false
         * @param fwInfoEl xml element
         * @return true if cause if component else false
         */
        private boolean isCauseFramework(Element fwInfoEl) {
            String value = "NO"; // true, false?
            
            Element isCauseFrameworkEl = DOMUtil.UTIL.getElement(fwInfoEl,
                    "is-cause-framework");
            if (isCauseFrameworkEl != null) {
                value = DOMUtil.UTIL.getTextData(isCauseFrameworkEl);
            } else {
                value = "NO";
            }
            // anything otherthan yes. is no.
            return value.equalsIgnoreCase("YES");
        }
        
        /**
         * return MessageInfo
         * @param msgInfoEl xml element.
         * @return MessageInfo
         */
        private MessageInfo getMessageInfo(Element msgInfoEl) {
            String id = UNKNOWN;
            String msg = null;
            List<String> params = new ArrayList<String>();
            
            Element idEl = DOMUtil.UTIL.getElement(msgInfoEl, "loc-token");
            
            if (idEl != null) {
                id = DOMUtil.UTIL.getTextData(idEl);
            }
            
            Element msgEl = DOMUtil.UTIL.getElement(msgInfoEl, "loc-message");
            
            if (msgEl != null) {
                msg = DOMUtil.UTIL.getTextData(msgEl);
            }
            
            NodeList paramElList = DOMUtil.UTIL.getChildElements(msgInfoEl,
                    "loc-param");
            int size = paramElList.getLength();
            params = new ArrayList<String>();
            for (int i = 0; i < size; ++i) {
                Element paramEl = (Element) paramElList.item(i);
                if (paramEl != null) {
                    String param = DOMUtil.UTIL.getTextData(paramEl);
                    params.add(param);
                }
            }
            
            return new MessageInfo(id, msg, params);
        }
        
        /**
         * returns ExceptionInfo
         * @param exInfoEl xml element.
         * @return ExceptionInfo
         */
        private ExceptionInfo getExceptionInfo(Element exInfoEl) {
            
            String level = "-1";
            MessageInfo msgInfo = null;
            String stacktrace = null;
            
            Element levelEl = DOMUtil.UTIL
                    .getElement(exInfoEl, "nesting-level");
            if (levelEl != null) {
                level = DOMUtil.UTIL.getTextData(levelEl);
            }
            
            Element msgInfoEl = DOMUtil.UTIL.getElement(exInfoEl,
                    "msg-loc-info");
            if (msgInfoEl != null) {
                msgInfo = getMessageInfo(msgInfoEl);
            }
            
            Element stacktraceEl = DOMUtil.UTIL.getElement(exInfoEl,
                    "stack-trace");
            if (stacktraceEl != null) {
                stacktrace = DOMUtil.UTIL.getTextData(stacktraceEl);
            }
            return new ExceptionInfo(level, msgInfo, stacktrace);
        }
        
        /**
         * return TaskResultInfo
         * @param taskResultInfoEl xml element.
         * @return TaskResultInfo.
         */
        private TaskResultInfo getTaskResultInfo(Element taskResultInfoEl) {
            
            String id = "UNKNOWN";
            String result = "UNKNOWN";
            String msgType = "UNKNOWN";
            List<MessageInfo> msgInfoList = new ArrayList<MessageInfo>();
            List<ExceptionInfo> exInfoList = new ArrayList<ExceptionInfo>();
            
            Element idEl = DOMUtil.UTIL.getElement(taskResultInfoEl, "task-id");
            if (idEl != null) {
                id = DOMUtil.UTIL.getTextData(idEl);
            }
            
            Element resultEl = DOMUtil.UTIL.getElement(taskResultInfoEl,
                    "task-result");
            if (resultEl != null) {
                result = DOMUtil.UTIL.getTextData(resultEl);
            }
            
            Element msgTypeEl = DOMUtil.UTIL.getElement(taskResultInfoEl,
                    "message-type");
            if (msgTypeEl != null) {
                msgType = DOMUtil.UTIL.getTextData(msgTypeEl);
            }
            
            //          Element msgInfoEl =
            //          DOMUtil.UTIL.getElement(taskResultInfoEl, "task-status-msg");
            //          if ( msgInfoEl != null )
            //          {
            //          statusMsgInfo = getMessageInfo(msgInfoEl);
            //          }
            // get status msg list
            
            NodeList msgInfoElList = DOMUtil.UTIL.getChildElements(
                    taskResultInfoEl, "task-status-msg");
            int msgInfoListSize = msgInfoElList.getLength();
            msgInfoList = new ArrayList<MessageInfo>();
            for (int i = 0; i < msgInfoListSize; ++i) {
                Element msgInfoEl = (Element) msgInfoElList.item(i);
                if (msgInfoEl != null) {
                    MessageInfo statusMsgInfo = getMessageInfo(msgInfoEl);
                    msgInfoList.add(statusMsgInfo);
                }
            }
            
            // get the exception info list
            NodeList exInfoElList = DOMUtil.UTIL.getChildElements(
                    taskResultInfoEl, "exception-info");
            int exInfoListSize = exInfoElList.getLength();
            exInfoList = new ArrayList<ExceptionInfo>();
            for (int i = 0; i < exInfoListSize; ++i) {
                Element exInfoEl = (Element) exInfoElList.item(i);
                if (exInfoEl != null) {
                    ExceptionInfo exInfo = getExceptionInfo(exInfoEl);
                    exInfoList.add(exInfo);
                }
            }
            
            return new TaskResultInfo(id, result, msgType, msgInfoList,
                    exInfoList);
        }
        
        /**
         * process the dom elements for the framework message and initilizes the
         * message object fields.
         * @param fwInfoEl framework message root element
         * @return FrameworkTaskResult object.
         */
        private FrameworkTaskResult getFrameworkTaskResult(Element fwInfoEl) {
            String locale = null;
            boolean isCuaseFramework = false;
            TaskResultInfo taskResultInfo = null;
            
            isCuaseFramework = isCauseFramework(fwInfoEl);
            locale = this.getFrameworkLocale(fwInfoEl);
            
            Element taskResultDetailsEl = DOMUtil.UTIL.getElement(fwInfoEl,
                    "task-result-details");
            taskResultInfo = getTaskResultInfo(taskResultDetailsEl);
            
            return new JBIManagementMessage.FrameworkTaskResult(taskResultInfo,
                    isCuaseFramework, locale);
        }
        
        /**
         * reads the dom tree for component messages and initializes the corresponding
         * fields of the Message object
         * @param compInfoEl component message info element
         * @return ComponentTaskResult
         */
        private ComponentTaskResult getComponentTaskResult(Element compInfoEl) {
            String compId = "UNKNOWN COMPONENT";
            TaskResultInfo taskResultInfo = null;
            
            Element compIdEl = DOMUtil.UTIL.getElement(compInfoEl,
                    "component-name");
            if (compIdEl != null) {
                compId = DOMUtil.UTIL.getTextData(compIdEl);
            }
            
            Element taskResultDetailsEl = DOMUtil.UTIL.getElement(compInfoEl,
                    "task-result-details");
            taskResultInfo = getTaskResultInfo(taskResultDetailsEl);
            
            return new JBIManagementMessage.ComponentTaskResult(taskResultInfo,
                    compId);
            
        }
    }
    
    /**
     * This is the Exception takes the Manamagement Message object and prints the
     * stacktrace information in the message as part of the stacktrace of the
     * exception.
     */
    public static class JBIManagementMessageException extends Exception {
        static final long serialVersionUID = -1L;
        
        /**
         * Message Object
         */
        JBIManagementMessage mMgmtMsg;
        
        /**
         * constructor that takes message Object
         * @param mgmtMsg message Object
         */
        public JBIManagementMessageException(JBIManagementMessage mgmtMsg) {
            this(mgmtMsg.getMessage(), mgmtMsg);
        }
        
        /**
         * constructor that takes custom message and the message object
         * @param msg custom message string
         * @param mgmtMsg message object
         */
        public JBIManagementMessageException(String msg,
                JBIManagementMessage mgmtMsg) {
            super(msg);
            this.mMgmtMsg = mgmtMsg;
        }
        
        /**
         * override method
         * @param s writer
         */
        public void printStackTrace(PrintWriter s) {
            this.mMgmtMsg.printStackTrace(s);
            super.printStackTrace(s);
        }
        
        /**
         * override method
         * @param s stream
         */
        public void printStackTrace(PrintStream s) {
            this.mMgmtMsg.printStackTrace(s);
            super.printStackTrace(s);
        }
        
    }
    
}
