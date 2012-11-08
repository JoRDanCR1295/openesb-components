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
 * @(#)DefaultDebugFrame.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELVariable;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugFrame;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebuggableEngine;

import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

public class DefaultDebugFrame implements DebugFrame {

    private static final Logger LOGGER = Logger.getLogger(DefaultDebugFrame.class.getName());

    DebugFrame remoteDebugFrame = null;

    String processInstanceId;

    String id;

    String bpelFile;

    String uri;

    String parentFrameId;

    boolean exited;

    public DefaultDebugFrame(String processInstanceId, String parentFrameId, String id,
            String bpelFile, String uri) {
        this.processInstanceId = processInstanceId;
        this.parentFrameId = parentFrameId;
        this.id = id;
        this.bpelFile = bpelFile;
        this.uri = uri;
    }

    private void discardRemoteFrame(Throwable t) {
        if (t.getMessage() != null) {
            LOGGER.log(Level.SEVERE, t.getMessage(), t);
        }
        LOGGER.log(Level.CONFIG, t.getClass().getName() + ", " + I18n.loc("BPCOR-4001: remoteDebugFrame detached"));

        synchronized (this) {
            remoteDebugFrame = null;
        }
    }

    public void setRemoteDebugger(BPELDebugger debugger) {
        synchronized (this) {
            try {
                if (debugger != null) {
                    this.remoteDebugFrame = debugger.enterFrame(id, processInstanceId,
                            parentFrameId, bpelFile, uri);
                } else {
                    this.remoteDebugFrame = null;
                }
            } catch (Throwable e) {
                discardRemoteFrame(e);
            }
        }
    }

    public void onExit(String bpelFileName, String uri) {
        if (!exited) {
            exited = true;
            try {
                if (remoteDebugFrame != null) {
                    remoteDebugFrame.onExit(bpelFileName, uri);
                }
            } catch (Throwable e) {
                discardRemoteFrame(e);
            }
        }

    }
    
    public void onLineChange(String bpelFileName, String uri, int line, String xpath,
            DebuggableEngine engine) {
        try {
            if (remoteDebugFrame != null) {
                remoteDebugFrame.onLineChange(bpelFileName, uri, line, xpath, engine);
            }
        } catch (Throwable e) {
            discardRemoteFrame(e);
        }
    }

    public void onTerminate(String bpelFileName, String uri, int lineNumber, String xpath) {
        try {
            if (remoteDebugFrame != null) {
                remoteDebugFrame.onTerminate(bpelFileName, uri, lineNumber, xpath);
            }
        } catch (Throwable e) {
            discardRemoteFrame(e);
        }
    }

    public void onXPathException(String bpelFileName, String uri, int line, String message,
            String xpath) {
        try {
            if (remoteDebugFrame != null) {
                remoteDebugFrame.onXPathException(bpelFileName, uri, line, message, xpath);
            }
        } catch (Throwable e) {
            discardRemoteFrame(e);
        }
    }

    public String getId() {
        // TODO Auto-generated method stub
        return id;
    }

    private static String removeNSOnXpath(String xpath) {
        final Pattern p = Pattern.compile("bpel_20:");
        Matcher matcher = p.matcher(xpath);
        return matcher.replaceAll("");
    }

    public String getProcessInstanceId() {
        // TODO Auto-generated method stub
        return processInstanceId;
    }

    public String getUri() {
        return uri;
    }

    public void onFault(String bpelFile, String uri, int lineNumber, String xpath,
            String faultName, BPELVariable faultData, DebuggableEngine engine) {
        try {
            if (remoteDebugFrame != null) {
                remoteDebugFrame.onFault(bpelFile, uri, lineNumber, xpath, faultName, faultData,
                        engine);
            }
        } catch (Throwable e) {
            discardRemoteFrame(e);
        }

    }

    public String getParentFrameId() {
        // TODO Auto-generated method stub
        return parentFrameId;
    }

    public void onActivityComplete(String bpelFile, String uri, int lineNumber, String xpath) {
        // TODO Auto-generated method stub
        try {
            if (remoteDebugFrame != null) {
                remoteDebugFrame.onActivityComplete(bpelFile, uri, lineNumber, xpath);
            }
        } catch (Throwable e) {
            discardRemoteFrame(e);
        }
    }
    
    public void onSubActivityComplete(String bpelFile, String uri, int lineNumber, String xpath) {
        // TODO Auto-generated method stub
        try {
            if (remoteDebugFrame != null) {
                remoteDebugFrame.onSubActivityComplete(bpelFile, uri, lineNumber, xpath);
            }
        } catch (Throwable e) {
            discardRemoteFrame(e);
        }
    }
}
