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
 * @(#)CmdAction.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractAction;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.view.App;
import com.sun.jbi.component.toolkit.project.view.MessagePanel;
import com.sun.jbi.component.toolkit.project.view.ProgressDialog;
import com.sun.jbi.component.toolkit.project.view.App.Status;

/**
 * Utility class to generate a progress dialog while {@link Cmd} is executing. 
 * @author Kevan Simpson
 */
public class CmdAction extends AbstractAction implements Exec {
        private Exec mExec;
        private ProgressDialog mProgressDlg;
        private App mApp;

        public CmdAction(App app, Exec exec) {
            this(exec.toString(), app, exec, -1);
        }
        
        public CmdAction(String desc, App app, Exec cmd, int mnemonic) {
            mApp = app;
            mExec = cmd;
            mProgressDlg = new ProgressDialog("asadmin", cmd.toString());
            if (mnemonic > 0) {
                putValue(MNEMONIC_KEY, mnemonic);
            }
            if (desc != null) {
                putValue(LONG_DESCRIPTION, desc);
            }
        }
        
        /** @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent) */
        public void actionPerformed(ActionEvent e) {
            showOutput(execute(mApp.getProject()));
        }

        /** @see com.sun.jbi.component.toolkit.project.util.Exec#execute(com.sun.jbi.component.toolkit.project.model.Project, java.lang.String[]) */
        public String[] execute(final Project proj, final String... args) {
//            long ts = System.currentTimeMillis();
            final List<String[]> list = new ArrayList<String[]>();
            (new Thread(new Runnable() {
                public void run() {
                    try {
                        String[] output = getExec().execute(proj, args);
                        list.add(output);
                    }
                    finally {
                        mProgressDlg.setVisible(false);
                    }
                }
            })).start();
            // show progress dialog
            mProgressDlg.setVisible(true);
            // dialog is modal, closed by AsAdmin completing
            String[] output = list.get(0);
//            System.out.println("asadmin duration = "+ (System.currentTimeMillis() - ts));
            return output;
        }
        
        protected App getApp() {
            return mApp;
        }
        
        protected Exec getExec() {
            return mExec;
        }
        protected String getCmdName() {
            return mExec.toString();
        }
        
        protected void showOutput(String... output) {
            MessagePanel.showMessageDialog(getCmdName(), Status.good, output);
            mApp.showMessages(Status.info, "Command "+ getCmdName() +" executed successfully!");
        }
    }