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
 * @(#)ProgressDialog.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.Frame;
import java.awt.GridLayout;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.border.EtchedBorder;

/**
 * 
 * @author Kevan Simpson
 */
public class ProgressDialog extends JDialog {
    private JProgressBar mPBar;
    
    public ProgressDialog(String title, String task) {
        super((Frame) null, title, true);
        
        getContentPane().setLayout(new GridLayout());
        JPanel pnl = new JPanel();
        pnl.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), task));
        mPBar = new JProgressBar(0, 100);
        mPBar.setString("running...");
        mPBar.setStringPainted(true);
        mPBar.setIndeterminate(true);
        pnl.add(mPBar);
        getContentPane().add(pnl);
        
        setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        pack();
        setLocationRelativeTo(null);
    }
}
