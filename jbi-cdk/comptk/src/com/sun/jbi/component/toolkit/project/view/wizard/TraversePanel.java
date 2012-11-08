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
 * @(#)TraversePanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JButton;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz;

/**
 * 
 * @author Kevan Simpson
 */
public class TraversePanel extends BaseWizPanel {
    private JButton mBackBtn, mNextBtn, mFinishBtn, mCancelBtn;
    
    /**
     * @param app
     * @param params
     */
    public TraversePanel(CreateWizard wiz) {
        super(wiz);
    }

    public void showStep(Wiz wiz) {
        enableButtons();
    }

    private void enableButtons() {
        int last = steps().length - 1;
        mBackBtn.setEnabled(current() > 0);
        mNextBtn.setEnabled(current() < last);
        mFinishBtn.setEnabled(!mNextBtn.isEnabled());
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        super.init(params);

        setLayout(new FlowLayout(FlowLayout.RIGHT));
        mBackBtn = new JButton(new AbstractAction("< Back"){
            public void actionPerformed(ActionEvent e) {
                wizard().traverse(-1);
            }
        });
        add(mBackBtn);
        mNextBtn = new JButton(new AbstractAction("Next >"){
            public void actionPerformed(ActionEvent e) {
                wizard().traverse(1);
            }
        });
        add(mNextBtn);
        mFinishBtn = new JButton(new AbstractAction("Finish"){
            public void actionPerformed(ActionEvent e) {
                wizard().traverse(Integer.MAX_VALUE);
            }
        });
        add(mFinishBtn);
        mCancelBtn = new JButton(new AbstractAction("Cancel"){
            public void actionPerformed(ActionEvent e) {
                wizard().traverse(Integer.MIN_VALUE);
            }
        });
        add(mCancelBtn);
        
        enableButtons();
    }

}
