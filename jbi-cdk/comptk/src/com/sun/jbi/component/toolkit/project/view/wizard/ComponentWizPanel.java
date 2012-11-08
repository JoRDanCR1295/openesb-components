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
 * @(#)ComponentWizPanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.scrub.Profile.ProjType;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.CompType;


/**
 * 
 * @author Kevan Simpson
 */
public class ComponentWizPanel extends EntryPanel {
    private WizardField mProjNmFld;
    
    /**
     * @param wiz
     */
    public ComponentWizPanel(CreateWizard wiz) {
        super(wiz);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#captureInput() */
    @Override
    protected boolean captureInput() {
        String profile = wizard().getTokens().get(Tkn.PROFILE),
               name = mProjNmFld.getValue().trim();
        if (verifyBasic(name)) {
            ProjType projType = ProjType.valueOf(profile);
            switch (projType) {
                case ojc: {
                    CompType compType = CompType.descriptionOf(
                            wizard().getTokens().get(Tkn.COMP_TYPE_DESC));
                    if (!name.endsWith(compType.getSuffix().toLowerCase())) {
                        JOptionPane.showMessageDialog(wizard(), 
                                compType.getDesc() +" projects must end with '"+ compType.getSuffix() +"'.", 
                                "Invalid Project Name!", JOptionPane.WARNING_MESSAGE);
                        return false;
                    }
                    else {
                        for (int i = 0, n = name.length(); i < n; i++) {
                            if (!Character.isLowerCase(name.charAt(i))) {
                                JOptionPane.showMessageDialog(wizard(), 
                                        projType.getDescription() +" projects must be lowercase.", 
                                        "Invalid Project Name!", JOptionPane.WARNING_MESSAGE);
                                return false;
                            }
                        }
                    }
                    break;
                }
                case contrib: {
                    if (!name.startsWith("contrib-")) {
                        JOptionPane.showMessageDialog(wizard(), 
                                projType.getDescription() +" projects must be start with 'contrib-'.", 
                                "Invalid Project Name!", JOptionPane.WARNING_MESSAGE);
                        return false;
                    }
                    break;
                }
                case external: {
                    // already validated non-empty
                    break;
                }
            }
            // validation complete, assign value
            wizard().getTokens().put(Tkn.PROJ_NAME, name);
            return true;
        }
        
        return false;
    }

    private boolean verifyBasic(String name) {
        if (Util.isEmpty(name) || name.contains(" ")) {
            JOptionPane.showMessageDialog(wizard(), 
                    "Project names cannot be empty or contain spaces!", 
                    "Invalid Project Name!", JOptionPane.WARNING_MESSAGE);
            return false;
        }
        return true;
    }
    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#createEntryPanel() */
    @Override
    protected JPanel createEntryPanel() {
        mProjNmFld = new WizardField(
                wizard(), Tkn.PROJ_NAME, "Project Name:",
                "The name of the project and its root folder.");
        return mProjNmFld;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#giveFocus() */
    @Override
    protected void giveFocus() {
        mProjNmFld.giveFocus();
    }

    @Override
    protected void updateInstructions(StyledDocument doc) throws BadLocationException {
        doc.insertString(0, "The name of the project and its root folder.\n\n", null);
        doc.insertString(doc.getLength(), "Conventions:", instructions().getStyle(Boolean.FALSE));
        doc.insertString(doc.getLength(), 
                "\n'OJC Core' - all lowercase, ending with \"bc\" or \"se\".\n"
                +"'Partner Contribution' - all lowercase, starting with \"contrib-\".\n"
                +"'Independent Projects' - no convention.", 
                null);
    }
}
