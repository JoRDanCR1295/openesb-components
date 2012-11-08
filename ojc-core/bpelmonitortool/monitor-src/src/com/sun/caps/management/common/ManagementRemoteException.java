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
package com.sun.caps.management.common;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.management.MBeanException;
import javax.management.RuntimeMBeanException;
import javax.management.RuntimeOperationsException;

import com.sun.caps.management.common.jbi.JBIManagementMessage;

/**
 * @author graj
 *
 */
public class ManagementRemoteException extends Exception implements
        Serializable {
    /** aCuase StackTrace */
    private StringBuffer causeStackTrace;

    /** aCuase Message trace */
    private String[]     causeMessageTrace;

    /**
     * Determines if a de-serialized file is compatible with this class.
     *
     * 1. Run serialver -show from the command line 2. Point the tool to the
     * class file including the package, for example:
     * com.sun.jbi.ui.common.ManagementRemoteException - without the .class The
     * serialver docs for Windows OS is at:
     * http://java.sun.com/j2se/1.5.0/docs/tooldocs/windows/serialver.html and
     * Unix OS is at:
     * http://java.sun.com/j2se/1.5.0/docs/tooldocs/solaris/serialver.html
     *
     * Maintainers must change this value if and only if the new version of this
     * class is not compatible with old versions. See Sun docs for <a
     * href=http://java.sun.com/products/jdk/1.1/docs/guide
     * /serialization/spec/version.doc.html> details. </a>
     */
    static final long    serialVersionUID = -1L;

    /**
     * Creates a new instance of ManagementRemoteException with an exception message.
     *
     * @param aMessage
     *            String describing this exception.
     */
    public ManagementRemoteException(String aMessage) {
        this(aMessage, (Throwable) null);
    }

    /**
     * Creates a new instance of ManagementRemoteException with the specified cause.
     *
     * @param aCause
     *            Throwable which represents an underlying problem (or null).
     */
    public ManagementRemoteException(Throwable aCause) {
        this((String) null, aCause);
    }

    /**
     * Creates a new instance of ManagementRemoteException with the specified message and
     * cause.
     *
     * @param aMessage
     *            String describing this exception.
     * @param aCause
     *            Throwable which represents an underlying problem (or null).
     */
    public ManagementRemoteException(String aMessage, Throwable aCause) {
        super(aMessage);
        initCauseTrace(aCause);
    }

    /**
     * initializes the stacktrace and messages from cause
     *
     * @param aCause
     *            a cause
     */
    public void initCauseTrace(Throwable aCause) {
        this.causeStackTrace = null;
        this.causeMessageTrace = null;
        if (aCause == null) {
            return;
        }

        // save the stack trace
        StringWriter traceWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(traceWriter);
        aCause.printStackTrace(printWriter);
        printWriter.close();
        this.causeStackTrace = traceWriter.getBuffer();

        List<String> list = new ArrayList<String>();

        for (Throwable nextCause = aCause; nextCause != null; nextCause = nextCause
                .getCause()) {
            list.add(nextCause.getMessage());
        }
        this.causeMessageTrace = (String[]) list.toArray(new String[0]);
    }

    /**
     * Returns the detail message string of this throwable.
     *
     * @return the detail message string of this <tt>Throwable</tt> instance
     *         (which may be <tt>null</tt>). + the cause messages
     */
    public String getMessage() {
        String mainMsg = super.getMessage();

        if (this.causeMessageTrace == null
                || this.causeMessageTrace.length <= 0) {
            return mainMsg;
        }

        boolean hasMainMsg = (mainMsg != null);

        StringWriter stringWriter = new StringWriter();
        PrintWriter msgWriter = new PrintWriter(stringWriter);
        if (hasMainMsg) {
            msgWriter.println(mainMsg);
            msgWriter.print(Util.getCommonI18NBundle().getMessage(
                    "caps.management.client.remote.exception.msg.root.cause.msg",
                    this.causeMessageTrace[0]));
        } else {
            msgWriter.print(this.causeMessageTrace[0]);
        }

        for (int i = 1; i < this.causeMessageTrace.length; ++i) {
            if((this.causeMessageTrace[i] != null) && (this.causeMessageTrace[i].trim().length() > 0)) {
                msgWriter.println();
                String message = Util.getCommonI18NBundle().getMessage(
                        "caps.management.client.remote.exception.msg.root.cause.msg",
                        this.causeMessageTrace[i]); 
                msgWriter.print(message);
            }
        }
        msgWriter.close();
        return stringWriter.toString();
    }

    /**
     * gets the cuase trace in a string buffer
     *
     * @return trace in a string buffer
     */
    public String[] getCauseMessageTrace() {
        return this.causeMessageTrace;
    }

    /**
     * gets the cuase trace in a string buffer
     *
     * @return trace in a string buffer
     */
    public StringBuffer getCauseStackTrace() {
        return this.causeStackTrace;
    }

    /**
     * override method
     *
     * @param s
     *            writer
     */
    public void printStackTrace(PrintWriter s) {
        super.printStackTrace(s);
        // s.println("Error Code : " + this.mErrorCode);
        synchronized (s) {
            if (this.causeStackTrace != null) {
                s.println(this.causeStackTrace.toString());
            }
        }
    }

    /**
     * override method
     *
     * @param s
     *            stream
     */
    public void printStackTrace(PrintStream s) {
        super.printStackTrace(s);
        synchronized (s) {
            if (this.causeStackTrace != null) {
                s.println(this.causeStackTrace.toString());
            }
        }
    }

    /**
     * retrieves the exception message and try to construct the jbi mgmt message
     *
     * @return JBIManagementMessage object
     */
    public JBIManagementMessage extractJBIManagementMessage() {
        String exMessage = null;
        String[] msgs = getCauseMessageTrace();
        if (msgs != null && msgs.length > 0) {
            exMessage = msgs[0];
        }

        if (exMessage == null) {
            return null;
        }

        JBIManagementMessage mgmtMsg = JBIManagementMessage
                .createJBIManagementMessage(exMessage);

        return mgmtMsg;
    }

    /**
     * filters the jmx exception and wraps the root cause user exception int the
     * ManagementRemoteException
     *
     * @param jmxEx
     *            exception
     * @return remote exception
     */
    public static ManagementRemoteException filterJmxExceptions(Exception jmxEx) {
        Throwable rootCause = jmxEx;
        Throwable cause = rootCause;
        for (;;) {
            if (cause == null
                    || !(cause instanceof MBeanException
                            || cause instanceof RuntimeMBeanException
                            || cause instanceof RuntimeOperationsException)) {
                break;
            }
            rootCause = cause.getCause();
            cause = rootCause;
        }
        if (rootCause instanceof ManagementRemoteException) {
            return (ManagementRemoteException) rootCause;
        } else {
            return new ManagementRemoteException(cause.getClass().getName(), rootCause);
        }
    }
}
