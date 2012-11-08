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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sun.caps.management.common.I18NBundle;
import com.sun.caps.management.common.jbi.JBIManagementMessage.ComponentTaskResult;
import com.sun.caps.management.common.jbi.JBIManagementMessage.ESBTaskResult;
import com.sun.caps.management.common.jbi.JBIManagementMessage.ExceptionInfo;
import com.sun.caps.management.common.jbi.JBIManagementMessage.FrameworkTaskResult;
import com.sun.caps.management.common.jbi.JBIManagementMessage.InstanceTaskResult;
import com.sun.caps.management.common.jbi.JBIManagementMessage.JBITaskResult;
import com.sun.caps.management.common.jbi.JBIManagementMessage.MessageInfo;
import com.sun.caps.management.common.jbi.JBIManagementMessage.TaskResultInfo;

/**
 * this class creates formatted text from the esb admin result.
 */
public class ESBResultFormatter {
    /**
     * same as cas msg format
     */
    public static final int    TOP_MSG_FORMAT     = 0;
    
    /**
     * cas msg format
     */
    public static final int    CAS_MSG_FORMAT     = 1;
    
    /**
     * instance msg format
     */
    public static final int    INST_MSG_FORMAT    = 2;
    
    /**
     * component msg format
     */
    public static final int    COMP_MSG_FORMAT    = 3;
    
    /**
     * failed status
     */
    public static final String FAILED             = "FAILED";
    
    /**
     * success status
     */
    public static final String SUCCESS            = "SUCCESS";
    
    /**
     * info message type
     */
    public static final String INFO               = "INFO";
    
    /**
     * warning message type
     */
    public static final String WARNING            = "WARNING";
    
    /**
     * error message type
     */
    public static final String ERROR              = "ERROR";
    
    /**
     * error message type
     */
    public static final String EXCEPTION          = "EXCEPTION";
    
    /**
     * unknown value for any unknonw value of any msg status or type.
     */
    public static final String UNKNOWN            = "UNKNOWN";
    
    /**
     * instance name marker in the msg/exp list
     */
    public static final String INSTANCE_MARKER_ID = "JBI_INSTANCE_NAME";
    
    /**
     * CAS msg/exp list key in the instance Maps
     */
    public static final String CAS_KEY            = "";
    
    public static final String OUT_NORMAL         = "normal";
    
    public static final String OUT_VERBOSE        = "terse";
    
    public static final String OUT_DEBUG          = "verbose";
    
    /**
     * print level
     */
    private String             mOutLevel          = OUT_VERBOSE;
    
    /**
     * resource bundle
     */
    private static I18NBundle  I18NBUNDLE        = null;
    
    public ESBResultFormatter() {
        this.mOutLevel = OUT_VERBOSE;
    }
    
    /** Creates a new instance of AdminResultFormatter */
    public ESBResultFormatter(String outLevel) {
        this.mOutLevel = outLevel;
    }
    
    /**
     * gives the I18N bundle
     * 
     * @return I18NBundle object
     */
    public static I18NBundle getI18NBundle() {
        // lazzy initialize the JBI Client
        if (I18NBUNDLE == null) {
            I18NBUNDLE = new I18NBundle("com.sun.jbi.ui.common");
        }
        return I18NBUNDLE;
    }
    
    /**
     * creates l10n message
     * 
     * @param code
     *            message code
     * @param msg
     *            message
     * @return formatted message
     */
    protected String getL10NMessage(String code, String msg) {
        // TODO: here is the chance to load and format the message using code
        // from the
        // client's resource bundle for displaying the msg in client locale.
        // For now, we stick with l10ned msg passed from the server.
        // Assumption is that the client and server is in the same locale
        return getI18NBundle().getMessage("esb.result.msg.format", code, msg);
    }
    
    /**
     * creates l10n message in exceptin format
     * 
     * @param code
     *            message code
     * @param msg
     *            message
     * @return formatted message
     */
    protected String getL10NExptionMessage(String code, String msg) {
        String formattedMsg = getL10NMessage(code, msg);
        return getI18NBundle().getMessage("esb.result.ex.msg.format",
                formattedMsg);
    }
    
    /**
     * message type from i18n bundle
     * 
     * @param msgType
     *            message type
     * @return message type
     */
    protected String getL10NMessageType(String msgType) {
        String l10nMsgType = null;
        if (ERROR.equalsIgnoreCase(msgType)) {
            l10nMsgType = getI18NBundle().getMessage("esb.result.type.error");
        } else if (WARNING.equalsIgnoreCase(msgType)) {
            l10nMsgType = getI18NBundle().getMessage("esb.result.type.warning");
        } else if (INFO.equalsIgnoreCase(msgType)) {
            l10nMsgType = getI18NBundle().getMessage("esb.result.type.info");
        } else {
            l10nMsgType = getI18NBundle().getMessage("esb.result.type.unknown");
        }
        return l10nMsgType;
    }
    
    /**
     * formats the message
     * 
     * @param msgType
     *            message type
     * @param code
     *            message code
     * @param msg
     *            message
     * @return formatted message
     */
    protected String getL10NMainMessage(String msgType, String code, String msg) {
        String formattedMsg = getL10NMessage(code, msg);
        String l10nType = getL10NMessageType(msgType);
        return getI18NBundle().getMessage("esb.result.1st.msg.format",
                l10nType, formattedMsg);
    }
    
    /**
     * message format type i18n key
     * 
     * @param formatType
     *            format type
     * @return i18n key
     */
    protected String getMainMessageI18NKey(int formatType) {
        switch (formatType) {
            case CAS_MSG_FORMAT:
                return "esb.result.cas.msg";
            case INST_MSG_FORMAT:
                return "esb.result.inst.msg";
            case COMP_MSG_FORMAT:
                return "esb.result.comp.msg";
            default:
                return "esb.result.cas.msg";
        }
    }
    
    /**
     * message format type i18n key
     * 
     * @param formatType
     *            format type
     * @return i18n key
     */
    protected String get2ndMessageI18NKey(int formatType) {
        switch (formatType) {
            case CAS_MSG_FORMAT:
                return "esb.result.cas.2nd.msg";
            case INST_MSG_FORMAT:
                return "esb.result.inst.2nd.msg";
            case COMP_MSG_FORMAT:
                return "esb.result.comp.2nd.msg";
            default:
                return "esb.result.cas.2nd.msg";
        }
    }
    
    /**
     * message format type i18n key
     * 
     * @param formatType
     *            format type
     * @return i18n key
     */
    protected String getExceptionMsgI18NKey(int formatType) {
        switch (formatType) {
            case CAS_MSG_FORMAT:
                return "esb.result.cas.ex.msg";
            case INST_MSG_FORMAT:
                return "esb.result.inst.ex.msg";
            case COMP_MSG_FORMAT:
                return "esb.result.comp.ex.msg";
            default:
                return "esb.result.cas.ex.msg";
        }
    }
    
    /**
     * message format type i18n key
     * 
     * @param formatType
     *            format type
     * @return i18n key
     */
    protected String get2ndExceptionMsgI18NKey(int formatType) {
        switch (formatType) {
            case CAS_MSG_FORMAT:
                return "esb.result.cas.2nd.ex.msg";
            case INST_MSG_FORMAT:
                return "esb.result.inst.2nd.ex.msg";
            case COMP_MSG_FORMAT:
                return "esb.result.comp.2nd.ex.msg";
            default:
                return "esb.result.cas.2nd.ex.msg";
        }
    }
    
    /**
     * prints message to the output
     * 
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgType
     *            message type
     * @param msgCode
     *            message code
     * @param msg
     *            message
     */
    protected void printMainMessage(PrintWriter out, int formatType,
            String msgType, String msgCode, String msg) {
        String fmtMsg = this.getL10NMainMessage(msgType, msgCode, msg);
        out.print(getI18NBundle().getMessage(
                this.getMainMessageI18NKey(formatType), fmtMsg));
    }
    
    /**
     * prints message to the output
     * 
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgCode
     *            message code
     * @param msg
     *            message
     */
    protected void print2ndMessage(PrintWriter out, int formatType,
            String msgCode, String msg) {
        String fmtMsg = this.getL10NMessage(msgCode, msg);
        out.print(getI18NBundle().getMessage(
                this.get2ndMessageI18NKey(formatType), fmtMsg));
    }
    
    /**
     * prints message to the output
     * 
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgCode
     *            message code
     * @param msg
     *            message
     */
    protected void printExceptionMessage(PrintWriter out, int formatType,
            String msgCode, String msg) {
        String fmtMsg = this.getL10NExptionMessage(msgCode, msg);
        out.print(getI18NBundle().getMessage(
                this.getExceptionMsgI18NKey(formatType), fmtMsg));
    }
    
    /**
     * prints message to the output
     * 
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgCode
     *            message code
     * @param msg
     *            message
     */
    protected void print2ndExceptionMessage(PrintWriter out, int formatType,
            String msgCode, String msg) {
        String fmtMsg = this.getL10NMessage(msgCode, msg);
        out.print(getI18NBundle().getMessage(
                this.get2ndExceptionMsgI18NKey(formatType), fmtMsg));
    }
    
    /**
     * prints the messages from message list
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgType
     *            message type
     * @param msgList
     *            list of TaskStatusMsgType objects
     */
    protected boolean printFormattedStatusMessages(PrintWriter out,
            int formatType, String msgType, List<MessageInfo> msgList) {
        boolean msgPrinted = false;
        
        if (msgList == null || msgList.size() == 0) {
            // do nothing
            return msgPrinted;
        }
        // print messages from msgList
        for (int msgIdx = 0; msgIdx < msgList.size(); ++msgIdx) {
            MessageInfo msgInfo = (MessageInfo) msgList.get(msgIdx);
            String msgCode = msgInfo.getI18nId();
            String msg = msgInfo.getLocalizedMsg();
            
            if (msgIdx == 0) {
                // print as main message
                printMainMessage(out, formatType, msgType, msgCode, msg);
            } else {
                out.println();
                print2ndMessage(out, formatType, msgCode, msg);
            }
            msgPrinted = true;
        }
        
        return msgPrinted;
    }
    
    /**
     * prints the messages from message list
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgType
     *            message type
     * @param expList
     *            list of ExceptionInfoType objects
     * @param printMainMsg
     *            true prints the first message as main message format
     */
    protected boolean printFormattedExceptionMessages(PrintWriter out,
            int formatType, String msgType, List<ExceptionInfo> expList,
            boolean printMainMsg) {
        boolean msgPrinted = false;
        boolean mainMsgPrinted = false;
        
        if (expList == null || expList.size() == 0) {
            // do nothing
            return msgPrinted;
        }
        
        // print messages from msgList
        for (int expIdx = 0; expIdx < expList.size(); ++expIdx) {
            ExceptionInfo expInfo = (ExceptionInfo) expList.get(expIdx);
            String msgCode = expInfo.getMessageInfo().getI18nId();
            String msg = expInfo.getMessageInfo().getLocalizedMsg();
            
            if (expIdx == 0) {
                // if asked for printing main message, format the first msg as
                // main message
                // else format the first msg as exception msg
                if (printMainMsg) {
                    // print as main message
                    printMainMessage(out, formatType, msgType, msgCode, msg);
                    mainMsgPrinted = true;
                } else {
                    out.println();
                    printExceptionMessage(out, formatType, msgCode, msg);
                }
            } else if (mainMsgPrinted && expIdx == 1) {
                // if main msg is printed print the second msg as exception
                // message
                // and print the subsequent exception messages ( 2,3,...) as
                // secondary messages
                out.println();
                printExceptionMessage(out, formatType, msgCode, msg);
            } else {
                // print second excpetion message
                out.println();
                print2ndExceptionMessage(out, formatType, msgCode, msg);
            }
            msgPrinted = true;
        }
        
        return msgPrinted;
    }
    
    /**
     * prints the messages from message list
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param msgType
     *            message type
     * @param msgList
     *            list of TaskStatusMsgType objects
     * @param expList
     *            list of ExceptionInfoType objects
     * @param startWithNewLine
     *            true prints the first message in new line
     */
    protected boolean printAllFormattedMessages(PrintWriter out,
            int formatType, String msgType, List<MessageInfo> msgList, List<ExceptionInfo> expList,
            boolean startWithNewLine) {
        boolean msgPrinted = false;
        boolean statusMsgPrinted = false;
        boolean expMsgPrinted = false;
        
        if ((msgList == null || msgList.size() == 0)
                && (expList == null || expList.size() == 0)) {
            // do nothing
            return msgPrinted;
        }
        
        if (startWithNewLine) {
            out.println();
        }
        statusMsgPrinted = this.printFormattedStatusMessages(out, formatType,
                msgType, msgList);
        boolean printMainMsg = false;
        if (!statusMsgPrinted) {
            printMainMsg = true;
        }
        // if not mainMsgPrinted print first exception message as main message
        expMsgPrinted = this.printFormattedExceptionMessages(out, formatType,
                msgType, expList, printMainMsg);
        
        msgPrinted = (statusMsgPrinted || expMsgPrinted);
        return msgPrinted;
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param out
     *            print writer to print the message
     * @param formatType
     *            format type
     * @param taskResultDetailsType
     *            TaskResultDetailsType object
     */
    public boolean printTaskResultMessages(PrintWriter out, int formatType,
            TaskResultInfo taskResultInfo) {
        boolean printed = false;
        boolean startWithNewLine = false;
        String msgType = taskResultInfo.getMessageType();
        List<MessageInfo> msgList = taskResultInfo.getStatusMessageInfoList();
        List<ExceptionInfo> expList = taskResultInfo.getExceptionInfoList();
        
        if ((msgList != null && msgList.size() > 0)
                || (expList != null && expList.size() > 0)) {
            printed = printAllFormattedMessages(out, formatType, msgType,
                    msgList, expList, startWithNewLine);
        }
        
        return printed;
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param taskResultDetailsType
     *            TaskResultDetailsType object
     * @param out
     *            print writer to print the message
     */
    public boolean printTaskResultMessages(PrintWriter out,
            TaskResultInfo taskResultInfo) {
        return printTaskResultMessages(out, this.TOP_MSG_FORMAT, taskResultInfo);
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param fwTaskResultType
     *            frmwkTaskResultType object
     * @param out
     *            print writer to print the message
     */
    public boolean printFrameworkLevelMessages(PrintWriter out,
            FrameworkTaskResult fwTaskResult) {
        TaskResultInfo taskResultInfo = fwTaskResult.getTaskResultInfo();
        return printTaskResultMessages(out, INST_MSG_FORMAT,
                taskResultInfo);
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param compTaskResultType
     *            ComponentTaskResultType object
     * @param out
     *            print writer to print the message
     */
    public boolean printComponentLevelMessages(PrintWriter out,
            ComponentTaskResult compTaskResult) {
        TaskResultInfo taskResultInfo = compTaskResult.getTaskResultInfo();
        return printTaskResultMessages(out, COMP_MSG_FORMAT,
                taskResultInfo);
    }
    
    /**
     * sort the list
     * 
     * @param compTaskResultList
     *            list.
     */
    public void sortComponentTaskResultList(
            List /* <ComponentTaskResult> */compTaskResultList) {
        try {
            Collections.sort(compTaskResultList, new Comparator() {
                public int compare(Object o1, Object o2) {
                    return ((ComponentTaskResult) o1).getComponentName()
                            .compareTo(
                                    ((ComponentTaskResult) o2)
                                            .getComponentName());
                }
            });
        } catch (ClassCastException ccEx) {
            // log and // do nothing.
        } catch (UnsupportedOperationException unsupEx) {
            // log and // do nothing.
        } catch (Exception ex) {
            // log and // do nothing
        }
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param jbiTaskResultType
     *            JbiTaskResultType object
     * @param out
     *            print writer to print the message
     */
    public boolean printJbiTaskResultMessages(PrintWriter out,
            JBITaskResult jbiTaskResult) {
        boolean msgPrinted = false;
        
        boolean fwMsgPrinted = false;
        boolean firstCompMsgPrinted = false;
        
        fwMsgPrinted = printFrameworkLevelMessages(out, jbiTaskResult
                .getFrameworkTaskResult());
        
        // print com msgs into comp writer
        List compList = jbiTaskResult.getComponentTaskResultList();
        sortComponentTaskResultList(compList);
        if (compList != null && compList.size() > 0) {
            for (int compIdx = 0; compIdx < compList.size(); ++compIdx) {
                boolean compMsgPrinted = false;
                StringWriter compBuff = new StringWriter();
                PrintWriter compOut = new PrintWriter(compBuff);
                ComponentTaskResult compTaskResult = (ComponentTaskResult) compList
                        .get(compIdx);
                compMsgPrinted = printComponentLevelMessages(compOut,
                        compTaskResult);
                compOut.close();
                if (!compMsgPrinted) {
                    continue;
                }
                // check if new line need to be printed
                if (firstCompMsgPrinted || fwMsgPrinted) {
                    out.println(); // printing a second message to the out
                }
                // comp msgs available.
                // print comp header ( comp name )
                String compName = compTaskResult.getComponentName();
                out.println(getI18NBundle().getMessage(
                        "esb.result.comp.msg.from.comp", compName));
                // print comp msgs to out
                out.print(compBuff.getBuffer().toString());
                firstCompMsgPrinted = true;
            }
        }
        
        msgPrinted = (fwMsgPrinted || firstCompMsgPrinted);
        return msgPrinted;
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param instResultDetailType
     *            InstanceResultDetailType object
     * @param printInstanceName
     *            true prints the instance name, false prints no instance name.
     * @param out
     *            print writer to print the message
     */
    public boolean printInstanceLevelMessages(PrintWriter out,
            InstanceTaskResult instResult, boolean printInstanceName) {
        boolean instMsgPrinted = false;
        StringWriter instBuff = new StringWriter();
        PrintWriter instOut = new PrintWriter(instBuff);
        
        String instanceName = instResult.getInstanceName();
        JBITaskResult jbiTaskResult = instResult.getJBITaskResult();
        instMsgPrinted = printJbiTaskResultMessages(instOut, jbiTaskResult);
        instOut.close();
        
        if (instMsgPrinted) {
            if (printInstanceName) {
                // print isntance name to out
                out.println(getI18NBundle().getMessage(
                        "esb.result.inst.msg.from.inst", instanceName));
            }
            // print instance msg to out
            out.print(instBuff.getBuffer().toString());
        }
        return instMsgPrinted;
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param instResultDetailType
     *            InstanceResultDetailType object
     * @param out
     *            print writer to print the message
     */
    public boolean printInstanceLevelMessages(PrintWriter out,
            InstanceTaskResult instResult) {
        boolean printInstanceName = true;
        return printInstanceLevelMessages(out, instResult, printInstanceName);
    }
    
    /**
     * sort the list
     * 
     * @param instResultList
     *            list.
     */
    public void sortInstanceResultList(
            List /* <ESBInstanceTaskResult> */instResultList) {
        try {
            Collections.sort(instResultList, new Comparator() {
                public int compare(Object o1, Object o2) {
                    return ((InstanceTaskResult) o1)
                            .getInstanceName()
                            .compareTo(
                                    ((InstanceTaskResult) o2).getInstanceName());
                }
            });
        } catch (ClassCastException ccEx) {
            // log and // do nothing.
        } catch (UnsupportedOperationException unsupEx) {
            // log and // do nothing.
        } catch (Exception ex) {
            // log and // do nothing
        }
    }
    
    /**
     * prints the messages
     * 
     * @return true if any message is printed, false if no msg is printed.
     * @param esbResultType
     *            EsbTaskResultType object
     * @param out
     *            print writer to print the message
     */
    public boolean printESBTaskResult(PrintWriter out, ESBTaskResult esbResult) {
        boolean msgPrinted = false;
        
        boolean casMsgPrinted = false;
        casMsgPrinted = printJbiTaskResultMessages(out, esbResult
                .getCASTaskResult());
        
        boolean firstInstMsgPrinted = false;
        
        // print Instance level messages
        List instList = esbResult.getInstanceTaskResultList();
        sortInstanceResultList(instList);
        if (instList != null && instList.size() > 0) {
            // no instance messages header as the instance name separates the
            // per instance messages
            for (int instIdx = 0; instIdx < instList.size(); ++instIdx) {
                boolean instMsgPrinted = false;
                StringWriter instBuff = new StringWriter();
                PrintWriter instOut = new PrintWriter(instBuff);
                InstanceTaskResult instResult = (InstanceTaskResult) instList
                        .get(instIdx);
                boolean printInstaceName = true;
                instMsgPrinted = printInstanceLevelMessages(instOut,
                        instResult, printInstaceName);
                instOut.close();
                if (!instMsgPrinted) {
                    continue;
                }
                // check if new line need to be printed
                if (firstInstMsgPrinted || casMsgPrinted) {
                    out.println(); // printing a second message to the out
                }
                // inst msg available here.
                // print inst msgs to out
                out.print(instBuff.getBuffer().toString());
                firstInstMsgPrinted = true;
            }
        }
        
        msgPrinted = (casMsgPrinted || firstInstMsgPrinted);
        return msgPrinted;
    }
    
    /**
     * This method iterates through the TaskResultInfo and sepearates the
     * messages into per instance msgs and expt in a map {<instanceName>, <List<MessageInfo>>}
     * or {<instanceName>, <List<ExceptionInfo>>}, if the messages are not
     * started with the instance marker message, they will be added into the
     * value against empty key in the Map which represent CAS level messages.
     * 
     * @return a Map <instancename, msg/expInfo List >
     */
    private Map getInstanceMessagesMap(TaskResultInfo taskResult,
            boolean isExpInfo) {
        
        List infoList = null;
        
        Map instMap = new HashMap();
        
        List instInfoList = null;
        String instanceName = CAS_KEY; // must initialize
        
        infoList = (isExpInfo) ? taskResult.getExceptionInfoList() : taskResult
                .getStatusMessageInfoList();
        
        if (infoList == null || infoList.isEmpty()) {
            // xml with no msg/expt info, but just has status. they go under CAS
            instInfoList = new ArrayList();
            instMap.put(CAS_KEY, instInfoList);
            return instMap;
        }
        
        for (int i = 0; i < infoList.size(); ++i) {
            
            Object info = infoList.get(i);
            MessageInfo msgInfo = (isExpInfo) ? ((ExceptionInfo) info)
                    .getMessageInfo() : (MessageInfo) info;
            
            String marker = msgInfo.getI18nId();
            
            if (INSTANCE_MARKER_ID.equals(marker)) {
                instanceName = msgInfo.getLocalizedMsg();
                instInfoList = (List) instMap.get(instanceName);
                // skip the marker msg info
                continue;
            }
            
            if (instInfoList == null) {
                instInfoList = new ArrayList();
                instMap.put(instanceName, instInfoList);
            }
            instInfoList.add(info);
        }
        
        return instMap;
    }
    
    /**
     * copy the id, resutl, msgtype from the taskresultinfo passed and create a
     * new TaskResultInfo with them and the passed msg, exp lists. This will be
     * used in re-creating the taskresultinfo per instance from the main jbi
     * mgmt msg task result info at framework level and component level
     */
    public TaskResultInfo createTaskResultInfo(TaskResultInfo taskResultInfo,
            List msgList, List expList) {
        
        List mList = msgList;
        List eList = expList;
        
        if (mList == null) {
            mList = new ArrayList();
        }
        if (eList == null) {
            eList = new ArrayList();
        }
        return new TaskResultInfo(taskResultInfo.getTaskId(), taskResultInfo
                .getResult(), taskResultInfo.getMessageType(), mList, eList);
    }
    
    public void updateInstanceMap(Map instMap,
            FrameworkTaskResult fwTaskResult, Map fwInstMsgMap, Map fwInstExpMap) {
        
        Set instSet = new HashSet();
        instSet.addAll(fwInstMsgMap.keySet());
        instSet.addAll(fwInstExpMap.keySet());
        
        for (Iterator itr = instSet.iterator(); itr.hasNext();) {
            String instName = (String) itr.next();
            
            InstanceTaskResult instResult = (InstanceTaskResult) instMap
                    .get(instName);
            if (instResult == null) {
                instResult = new InstanceTaskResult();
                instResult.setInstanceName(instName);
                instMap.put(instName, instResult);
            }
            
            TaskResultInfo taskResultInfo = createTaskResultInfo(fwTaskResult
                    .getTaskResultInfo(), (List) fwInstMsgMap.get(instName),
                    (List) fwInstExpMap.get(instName));
            FrameworkTaskResult instFwTaskResult = new FrameworkTaskResult(
                    taskResultInfo, fwTaskResult.isCauseFramework(),
                    fwTaskResult.getLocale());
            instResult.getJBITaskResult().setFrameworkTaskResult(
                    instFwTaskResult);
            
        }
    }
    
    public void updateInstanceMap(Map instMap, ComponentTaskResult compResult,
            Map msgMap, Map expMap) {
        
        Set instSet = new HashSet();
        instSet.addAll(msgMap.keySet());
        instSet.addAll(expMap.keySet());
        
        for (Iterator itr = instSet.iterator(); itr.hasNext();) {
            String instName = (String) itr.next();
            InstanceTaskResult instResult = (InstanceTaskResult) instMap
                    .get(instName);
            if (instResult == null) {
                instResult = new InstanceTaskResult();
                instResult.setInstanceName(instName);
                instMap.put(instName, instResult);
            }
            
            TaskResultInfo taskResultInfo = createTaskResultInfo(compResult
                    .getTaskResultInfo(), (List) msgMap.get(instName),
                    (List) expMap.get(instName));
            ComponentTaskResult instCompTaskResult = new ComponentTaskResult(
                    taskResultInfo, compResult.getComponentName());
            instResult.getJBITaskResult().getComponentTaskResultList().add(
                    instCompTaskResult);
        }
    }
    
    public ESBTaskResult toESBTaskResult(JBIManagementMessage jbiMsg) {
        
        FrameworkTaskResult fwResult = jbiMsg.getFrameworkTaskResult();
        List compResultList = jbiMsg.getComponentTaskResultList();
        
        TaskResultInfo fwTaskResultInfo = fwResult.getTaskResultInfo();
        Map fwInstMsgMap = getInstanceMessagesMap(fwTaskResultInfo, false); // get
                                                                            // status
                                                                            // msg
        Map fwInstExpMap = getInstanceMessagesMap(fwTaskResultInfo, true); // get
                                                                            // status
                                                                            // msg
        
        // create instance level framewrok task result
        Map instMap = new HashMap();
        updateInstanceMap(instMap, fwResult, fwInstMsgMap, fwInstExpMap);
        // update component results per instance
        for (int i = 0; i < compResultList.size(); ++i) {
            ComponentTaskResult compResult = (ComponentTaskResult) compResultList
                    .get(i);
            Map compInstMsgMap = getInstanceMessagesMap(compResult
                    .getTaskResultInfo(), false); // get statusp msg map
            Map compInstExpMap = getInstanceMessagesMap(compResult
                    .getTaskResultInfo(), true); // get exp msg map
            updateInstanceMap(instMap, compResult, compInstMsgMap,
                    compInstExpMap);
        }
        
        // now, the instance map contains the instance result with key CAS_KEY
        // which is CAS level messages. So,
        // remove them from the instance level messages and keep it as a CAS
        // result
        InstanceTaskResult casInstResult = (InstanceTaskResult) instMap
                .get(CAS_KEY);
        instMap.remove(CAS_KEY);
        
        List instTaskResultList = new ArrayList(instMap.values());
        JBITaskResult casTaskResult = null;
        if (casInstResult != null) {
            casTaskResult = casInstResult.getJBITaskResult();
        } else {
            casTaskResult = new JBITaskResult();
        }
        ESBTaskResult esbTaskResult = new ESBTaskResult(casTaskResult,
                instTaskResultList);
        
        return esbTaskResult;
    }
    
    /**
     * creates formatted esb results
     * 
     * @param esbResult
     *            esb results object
     * @return formatted text
     */
    public String getFormattedResult(ESBTaskResult esbResult) {
        
        if (esbResult == null) {
            return null;
        }
        
        String formattedMsg = null;
        
        StringWriter buff = new StringWriter();
        PrintWriter out = new PrintWriter(buff);
        
        // print ESB Result messages
        printESBTaskResult(out, esbResult);
        
        // buff.close(); // has no effect as it is a string writer.
        out.close();
        formattedMsg = buff.getBuffer().toString();
        
        return formattedMsg;
    }
    
    /**
     * return the formatted message if you can create. else return null
     * 
     * @param jbiMgmtMsgXml
     *            string contains the xml confirms to the JbiManagementMessage
     *            schema
     * @return the formatted message if you can create. else return null
     */
    public String getFormattedESBResult(JBIManagementMessage jbiMsg) {
        
        if (jbiMsg == null) {
            return null;
        }
        
        String formattedMsg = null;
        ESBTaskResult esbTaskResult = toESBTaskResult(jbiMsg);
        formattedMsg = getFormattedResult(esbTaskResult);
        
        return formattedMsg;
    }
    
    /**
     * return the formatted message if you can create. else return null
     * @param jbiMgmtMsgXml string contains the xml confirms to the JbiManagementMessage schema
     * @return the formatted message if you can create. else return null
     */
    public String getFormattedESBResult(String jbiMgmtMsgXml) {
        JBIManagementMessage jbiMsg = JBIManagementMessage
                .createJBIManagementMessage(jbiMgmtMsgXml);
        return getFormattedESBResult(jbiMsg);
    }
    
}
