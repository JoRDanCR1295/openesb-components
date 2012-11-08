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
 * @(#)EntryPanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import com.sun.jbi.component.toolkit.project.util.I18n;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz;


/**
 * 
 * @author Kevan Simpson
 */
public abstract class EntryPanel extends BaseWizPanel {
    private HelpPanel mInstructionsPnl;
    
    /**
     * @param app
     * @param params
     */
    public EntryPanel(Object... params) {
        super(params);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.BaseWizPanel#showStep(com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz) */
    @Override
    public void showStep(Wiz wiz) {
        // do nothing
    }

    protected HelpPanel instructions() {
        return mInstructionsPnl;
    }
    
    protected abstract boolean captureInput();
    protected abstract JPanel createEntryPanel();
    protected abstract void giveFocus();
    protected abstract void updateInstructions(StyledDocument doc) throws BadLocationException;
 
    /** @see com.sun.jbi.component.toolkit.project.view.wizard.BaseWizPanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        super.init(params);
        setLayout(new BorderLayout(5, 5));
        add(createEntryPanel(), BorderLayout.NORTH);
        mInstructionsPnl = new HelpPanel(wizard(), true) {
            @Override
            protected void updateHelp(Wiz wiz) {
                if (instructions() != null) {
                    try {
                        EntryPanel.this.updateInstructions(instructions().document());
                    }
                    catch (BadLocationException ble) {
                        throw badLocation(ble);
                    }

                }
            }
        };
        add(mInstructionsPnl, BorderLayout.CENTER);
        try { updateInstructions(instructions().document()); }
        catch (BadLocationException ble) { throw badLocation(ble); }
    }
    
    private ProjectException badLocation(BadLocationException ble) {
        throw new ProjectException(I18n.loc(    // XXX
                "Failed to update instructions {0}: {1}", 
                steps()[current()], ble.getMessage()));
    }
}
