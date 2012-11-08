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
 * @(#)HelpPanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import java.awt.Font;
import java.awt.GridLayout;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import com.sun.jbi.component.toolkit.project.util.I18n;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.util.Selection;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz;

/**
 * 
 * @author Kevan Simpson
 */
public class HelpPanel extends BaseWizPanel {
    private JTextPane mHelpArea;
    private Selection[] mSelections;
    
    /**
     * @param wiz
     */
    public HelpPanel(CreateWizard wiz, boolean scroll) {
        super(wiz, scroll);
    }

    public void showStep(Wiz wiz) {
        updateHelp(wiz);
    }

    protected StyledDocument document() {
        return mHelpArea.getStyledDocument();
    }
    
    protected Style getStyle(Boolean bold) {
        if (bold == null) {
            return null;
        }
        
        StyledDocument doc = mHelpArea.getStyledDocument();
        String style = "CDKHelp"+ bold.toString();
        Style cdk = doc.getStyle(style);
        if (cdk == null) {
            cdk = doc.addStyle(style, null);
            if (bold.booleanValue()) {
                StyleConstants.setBold(cdk, true);
            }
            else {
                StyleConstants.setUnderline(cdk, true);
            }
        }

        return cdk;
    }

    protected boolean replace(Selection sel, Style style) {
        try {
            StyledDocument doc = mHelpArea.getStyledDocument();
            int offset = sel.getOffset(), len = sel.getLength();
            // remove current selection
            doc.remove(offset, len);
            // remove highlight
            doc.insertString(offset, sel.getText(), style);

            return true;
        }
        catch (BadLocationException ble) {
            throw new ProjectException(I18n.loc(    // XXX
                    "Failed to replace text in Help panel: {0}", 
                    ble.getMessage()));
        }
    }

    protected void updateHelp(Wiz wiz) {
        try {
            if (wiz == null) {
                // load help text
                StyledDocument doc = mHelpArea.getStyledDocument();
                String title = "Steps                                             ";
                doc.insertString(0, title, getStyle(Boolean.FALSE));
                StringBuffer buff = new StringBuffer();
                buff.append("\n\n");
                for (Wiz w : Wiz.values()) {
                    buff.append(w.ordinal() + 1).append(".    ")
                        .append(w.getHelp()).append("\n");
                }
                
                doc.insertString(title.length(), buff.toString(), null);
                
                // init Selections
                String text = doc.getText(0, doc.getLength());
                mSelections = new Selection[Wiz.values().length];
                for (Wiz w : Wiz.values()) {
                    String re = String.valueOf(w.ordinal() + 1) +".*"+ w.getHelp();
                    Matcher m = Pattern.compile(re).matcher(text);
                    m.find();
                    mSelections[w.ordinal()] = new Selection(m);
                }
            }
            else {
                replace(mSelections[previous()], null);
                replace(mSelections[wiz.ordinal()], getStyle(Boolean.TRUE));
            }
        }
        catch (BadLocationException ble) {
            throw new ProjectException(I18n.loc(    // XXX 
                    "Failed to update Help panel: {0}", 
                    ble.getMessage()));
        }
    }
    
    @Override
    protected void init(Object... params) {
        super.init(params);
        boolean addScroll = ((Boolean) params[1]).booleanValue();
        setLayout(new GridLayout());
        
        mHelpArea = new JTextPane();
        mHelpArea.setFont(new Font("Dialog", Font.PLAIN, 14));
        mHelpArea.setEditable(false);
        mHelpArea.setFocusable(false);
        // to size text pane...
        JTextArea area = new JTextArea(8, 25);
        mHelpArea.setPreferredSize(area.getPreferredSize());
        mHelpArea.setMaximumSize(area.getPreferredSize());
        
        if (addScroll) {
            JScrollPane scroll = new JScrollPane(mHelpArea, 
                    JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                    JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            scroll.setMaximumSize(scroll.getPreferredSize());
            add(scroll);
        }
        else {
            add(mHelpArea);
        }
        
        setFocusable(false);
        updateHelp(null);
        updateHelp(Wiz.start);
    }
}
