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
import java.util.List;

import com.sun.caps.management.common.DOMUtil;
import com.sun.caps.management.common.I18NBundle;

/**
 * This class is used to create a JBI Management message that contains the
 * messages and exception under the framework tags to help the ui messages
 * formatted in the standard message format as the any other result messages
 * comming from the jbi runtime in the form of jbi manamgment message xml.
 */
public class JBIResultXmlBuilder {

    /**
     * success result
     */
    public final static boolean        SUCCESS_RESULT       = true;

    /**
     * failed result
     */
    public final static boolean        FAILED_RESULT        = false;

    /**
     * info message type
     */
    public final static String         INFO_MSG_TYPE        = "INFO";

    /**
     * warning message type
     */
    public final static String         WARNING_MSG_TYPE     = "WARNING";

    /**
     * error message type
     */
    public final static String         ERROR_MSG_TYPE       = "ERROR";

    /**
     * default task id
     */
    public final static String         DEFAULT_TASK_ID      = "UI_COMMON_TASK";

    /**
     * default message code
     */
    public final static String         DEFAULT_MSG_CODE     = "UICMN0000";

    /**
     * jbi mgmt xml prefix
     */
    private final static String        JBI_MGMT_XML_BEGIN   = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
                                                                    + "<jbi-task version=\"1.0\" "
                                                                    + "xmlns=\"http://java.sun.com/xml/ns/jbi/management-message\" "
                                                                    + "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" > "
                                                                    + "<jbi-task-result>"
                                                                    + "<frmwk-task-result>"
                                                                    + "<frmwk-task-result-details>"
                                                                    + "<task-result-details>";

    /**
     * jbi mgmt xml end
     */
    private final static String        JBI_MGMT_XML_END     = "</task-result-details>"
                                                                    + "<locale></locale>"
                                                                    + "</frmwk-task-result-details>"
                                                                    + "<is-cause-framework>YES</is-cause-framework>"
                                                                    + "</frmwk-task-result>"
                                                                    + "</jbi-task-result>"
                                                                    + "</jbi-task>";

    /**
     * static instance of this class
     */
    private static JBIResultXmlBuilder sJbiResultXmlBuilder = null;

    /** Creates a new instance of EsbResultXmlBuilder */
    protected JBIResultXmlBuilder() {
    }

    /**
     * returns the instance of this class
     *
     * @return instance of this class
     */
    public static JBIResultXmlBuilder getInstance() {
        if (sJbiResultXmlBuilder == null) {
            sJbiResultXmlBuilder = new JBIResultXmlBuilder();
        }
        return sJbiResultXmlBuilder;
    }

    /**
     * returns the exception cause chain in a list including this exception
     *
     * @param ex
     *            exception
     * @return list contain the exception chain
     */
    protected List<Throwable> flatExceptionChain(Throwable ex) {
        List<Throwable> list = new ArrayList<Throwable>();
        list.add(ex);
        for (Throwable cause = ex.getCause(); cause != null; cause = cause
                .getCause()) {
            list.add(cause);
        }
        return list;
    }

    /**
     * return the xml chunk that has task info elements
     *
     * @param taskId
     *            task id
     * @param successResult
     *            true for SUCCESS, false for FAILED
     * @param msgType
     *            one of INFO, ERROR, WARNING strings
     * @return the xml chunk
     */
    protected String getTaskInfoXml(String taskId, boolean successResult,
            String msgType) {
        String taskResult = (successResult) ? "SUCCESS" : "FAILED";

        return "<task-id>" + taskId + "</task-id>" + "<task-result>"
                + taskResult + "</task-result>" + "<message-type>" + msgType
                + "</message-type>";
    }

    /**
     * return the xml chunk contains the message info elements
     *
     * @param msgCode
     *            i18n message code
     * @param msg
     *            l10n message
     * @param args
     *            arguments for i18n message
     * @return the xml chunk
     */
    protected String getMsgLocInfoXml(String msgCode, String msg, Object[] args) {
        StringBuffer buff = new StringBuffer();

        buff.append("<msg-loc-info>" + "<loc-token>" + msgCode + "</loc-token>"
                + "<loc-message>"
                + DOMUtil.replaceXmlEscapeCharsToEntityRefereces(msg)
                + "</loc-message>");
        if (args != null) {
            for (int i = 0; i < args.length; ++i) {
                buff.append("<loc-param>" + args[i] + "</loc-param>");
            }
        }
        buff.append("</msg-loc-info>");

        return buff.toString();

    }

    /**
     * return the xml chuck for status message elements
     *
     * @param msgCode
     *            i18n message code
     * @param msg
     *            l10n message
     * @param args
     *            arguments for i18n message
     * @return the xml chunk
     */
    protected String getTaskStatusMsgXml(String msgCode, String msg,
            Object[] args) {
        return "<task-status-msg>" + getMsgLocInfoXml(msgCode, msg, args)
                + "</task-status-msg>";
    }

    /**
     * return the excpetion stacktrace as a string
     *
     * @param ex
     *            exception
     * @return string contains the exception stacktrace
     */
    protected String getExceptionStackTrace(Throwable ex) {

        StringWriter buff = new StringWriter();
        PrintWriter out = new PrintWriter(buff);
        String stackTrace = "";

        StackTraceElement[] stEl = ex.getStackTrace();
        if (stEl == null || stEl.length == 0) {
            return stackTrace;
        }
        for (int i = 0; i < stEl.length; ++i) {
            out.println(stEl[i].toString());
        }
        out.close();
        stackTrace = buff.getBuffer().toString();

        return stackTrace;
    }

    /**
     * returns exceptin info xml chunk
     *
     * @param msgCode
     *            i18n message code
     * @param nestingLevel
     *            level in the exception chain
     * @param ex
     *            exception for stacktrace
     * @return xml chunk
     */
    protected String getExceptionInfoXml(String msgCode, int nestingLevel,
            Throwable ex) {
        String stackTrace = getExceptionStackTrace(ex);

        return "<exception-info>" + "<nesting-level>" + nestingLevel
                + "</nesting-level>"
                + getMsgLocInfoXml(msgCode, ex.getMessage(), null)
                + "<stack-trace>"
                + DOMUtil.replaceXmlEscapeCharsToEntityRefereces(stackTrace)
                + "</stack-trace>" + "</exception-info>";
    }

    /**
     * returns the xml chunk contains exception message info for the exception
     * chain
     *
     * @param msgCode
     *            i18n code
     * @param ex
     *            exception
     * @return xml chunk
     */
    protected String getChainedExceptionInfoXml(String msgCode, Throwable ex) {
        StringBuffer buff = new StringBuffer();

        if (msgCode == null) {
            msgCode = DEFAULT_MSG_CODE; // UNKNOWN MSG CODE
        }
        List<Throwable> list = this.flatExceptionChain(ex);
        int listSize = list.size();
        for (int i = 0; i < listSize; ++i) {
            buff.append(getExceptionInfoXml(msgCode, i + 1, (Throwable) list
                    .get(i)));
        }

        return buff.toString();
    }

    /**
     * creates the jbi management xml
     *
     * @param taskId
     *            task id
     * @param successResult
     *            true for SUCCESS, false for FAILIED message
     * @param msgType
     *            one of INFO, ERROR, WARNING
     * @param msgCode
     *            i18n message code
     * @param msg
     *            message
     * @param args
     *            i18n message arguments
     * @param ex
     *            exception
     * @return jbi mgmt xml
     */
    public String createJbiResultXml(String taskId, boolean successResult,
            String msgType, String msgCode, String msg, Object[] args,
            Throwable ex) {
        StringBuffer buff = new StringBuffer();

        buff.append(JBI_MGMT_XML_BEGIN);
        buff.append(getTaskInfoXml(taskId, successResult, msgType));
        buff.append(getTaskStatusMsgXml(msgCode, msg, args));
        if (ex != null) {
            String expMsgCode = DEFAULT_MSG_CODE;
            if (msg == null) {
                // if msg is null, assume msgCode passed is for expception
                expMsgCode = msgCode;
            }
            buff.append(getChainedExceptionInfoXml(expMsgCode, ex));
        }
        buff.append(JBI_MGMT_XML_END);

        return buff.toString();
    }

    /**
     * creates the jbi management xml
     *
     * @param taskId
     *            task id
     * @param successResult
     *            true for SUCCESS, false for FAILIED message
     * @param msgType
     *            one of INFO, ERROR, WARNING
     * @param msgCode
     *            i18n message code
     * @param msg
     *            message
     * @param args
     *            i18n message arguments
     * @return jbi mgmt xml
     */
    public String createJbiResultXml(String taskId, boolean successResult,
            String msgType, String msgCode, String msg, Object[] args) {
        return createJbiResultXml(taskId, successResult, msgType, msgCode, msg,
                args, null);
    }

    /**
     * creates the jbi management xml
     *
     * @param taskId
     *            task id
     * @param msgCode
     *            i18n message code
     * @param ex
     *            exception
     * @return jbi mgmt xml
     */
    public String createJbiResultXml(String taskId, String msgCode, Throwable ex) {
        return createJbiResultXml(taskId, false, "ERROR", msgCode, null, null,
                ex);
    }

    /**
     * creates the jbi management xml
     *
     * @param i18nBundle
     *            i18n bundle object
     * @param i18nKey
     *            key to look for i18n msg
     * @param args
     *            i18n args
     * @param taskId
     *            task id
     * @param successResult
     *            true or false
     * @param msgType
     *            message type
     * @param ex
     *            exception
     * @return the jbi management xml
     */
    public static String createJbiResultXml(I18NBundle i18nBundle,
            String i18nKey, Object[] args, String taskId,
            boolean successResult, String msgType, Throwable ex) {

        String msgCode = i18nBundle.getMessage(i18nKey + ".ID");
        String msg = i18nBundle.getMessage(i18nKey, args);

        String jbiResultXml = JBIResultXmlBuilder.getInstance()
                .createJbiResultXml(taskId, successResult, msgType, msgCode,
                        msg, args, ex);

        if (jbiResultXml == null) {
            return msg;
        } else {
            return jbiResultXml;
        }

    }

    /**
     * creates the jbi management xml
     *
     * @param i18nBundle
     *            i18n bundle object
     * @param i18nKey
     *            i18n key in the bundle
     * @param args
     *            i18n args
     * @param ex
     *            exception
     * @return jbi mgmt xml
     */
    public static String createJbiResultXml(I18NBundle i18nBundle,
            String i18nKey, Object[] args, Throwable ex) {
        return createJbiResultXml(i18nBundle, i18nKey, args,
                "CAPS_MANAGEMENT_CLIENT_TASKS", JBIResultXmlBuilder.FAILED_RESULT,
                JBIResultXmlBuilder.ERROR_MSG_TYPE, ex);
    }

    /**
     * creates the jbi management xml
     *
     * @param i18nBundle
     *            i18n bundle object
     * @param i18nKey
     *            i18n key in the bundle
     * @param args
     *            i18n args
     * @return jbi mgmt xml
     */
    public static String createFailedJbiResultXml(I18NBundle i18nBundle,
            String i18nKey, Object[] args) {
        return createJbiResultXml(i18nBundle, i18nKey, args,
                "CAPS_MANAGEMENT_CLIENT_TASKS", JBIResultXmlBuilder.FAILED_RESULT,
                JBIResultXmlBuilder.ERROR_MSG_TYPE, null);
    }

}
