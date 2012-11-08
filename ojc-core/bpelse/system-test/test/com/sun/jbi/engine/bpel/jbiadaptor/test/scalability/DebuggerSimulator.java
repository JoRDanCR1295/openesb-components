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
 * @(#)DebuggerSimulator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.bpel.jbiadaptor.test.scalability;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELVariable;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugFrame;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebuggableEngine;

import com.sun.jbi.engine.bpel.core.bpel.debug.DefaultDebugger;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.CrashpointInfo;

public class DebuggerSimulator extends  DefaultDebugger {

    CrashpointInfo mCrashpointInfo;
    
    public DebuggerSimulator(Engine engine, CrashpointInfo crashpoint) {
        super(engine);
        mCrashpointInfo = crashpoint;
    }

    public DebugFrame enterFrame(String id, String rootFrameId, String parentFrameId, String bpelFile, String uri) {
        return new DebuggerFrame(rootFrameId, parentFrameId, id);
    }

    class DebuggerFrame implements DebugFrame {

        String parentFrameId;
        String rootFrameId;
        String id;

        public DebuggerFrame(String parentFrameId, String rootFrameId, String id) {
            super();
            this.parentFrameId = parentFrameId;
            this.rootFrameId = rootFrameId;
            this.id = id;
        }

        public void onLineChange(String bpelFile, String uri, int lineNumber, String xPath, DebuggableEngine engine) {
            if (mCrashpointInfo.isCrashEnabled() && mCrashpointInfo.getBpelFileName().equals(bpelFile)
                    && mCrashpointInfo.getCrashpoint() == lineNumber) {
                System.out.println("Crashing Instance for " + bpelFile + " during excution of Line number : " + lineNumber);
                Thread.currentThread().interrupt();
                throw new RuntimeException();
            }
        }

        public void onFault(String arg0,
                            String arg1,
                            int arg2,
                            String arg3,
                            String arg4,
                            BPELVariable arg5,
                            DebuggableEngine arg6) {
            // TODO Auto-generated method stub
        }

        public void onFault(String arg0, String arg1, int arg2, String arg3) {
            // TODO Auto-generated method stub
        }

        public void onXPathException(String arg0, String arg1, int arg2, String arg3, String arg4) {
            // TODO Auto-generated method stub
        }

        public void onTerminate(String arg0, String arg1, int arg2, String arg3) {
            // TODO Auto-generated method stub
        }

        public void onExit(String arg0, String arg1) {
            // TODO Auto-generated method stub
        }

        public void onActivityComplete(String arg0, String arg1, int arg2, String arg3) {
            // TODO Auto-generated method stub
        }

        public void onSubActivityComplete(String bpelFile, String uri, int lineNumber, String xpath) {
        }
    }
}