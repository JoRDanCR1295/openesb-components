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
 * @(#)PomWizPanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import javax.swing.JPanel;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.view.DescriptionField.Text;


/**
 * 
 * @author Kevan Simpson
 */
public class PomWizPanel extends EntryPanel {

    /**
     * @param wiz
     */
    public PomWizPanel(CreateWizard wiz) {
        super(wiz);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#captureInput() */
    @Override
    protected boolean captureInput() {
        return false;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#createEntryPanel() */
    @Override
    protected JPanel createEntryPanel() {
        WizardField fld = new WizardField(
                wizard(), Tkn.COMP_TYPE_DESC, "Component Type",
                "Service Engines provide the implementation of web services while\n"
                +" Binding Components act as proxies by exposing web services within\n"
                +" and external to the message bus.",
                Text.radio, new String[] { "Service Engine", "Binding Component" });
        return fld;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#giveFocus() */
    @Override
    protected void giveFocus() {
//        mCompTypeFld.giveFocus();
    }

    @Override
    protected void updateInstructions(StyledDocument doc) throws BadLocationException {
        doc.insertString(0, "Service Engines", instructions().getStyle(Boolean.TRUE));
        doc.insertString(doc.getLength(), 
                " provide the implementation of web services.\n"
                +"Examples include BPELSE, which exposes BPEL processes as web services, "
                +"and XsltSE, which provisions transformation processes.\n\n", 
                null);
        doc.insertString(doc.getLength(), "Binding Components", instructions().getStyle(Boolean.TRUE));
    }
}
