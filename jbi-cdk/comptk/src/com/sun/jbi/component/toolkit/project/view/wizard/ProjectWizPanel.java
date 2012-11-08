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
 * @(#)ProjectWizPanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.AbstractButton;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.CDK;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.scrub.Profile.ProjType;
import com.sun.jbi.component.toolkit.project.view.DescriptionField.Text;


/**
 * 
 * @author Kevan Simpson
 */
public class ProjectWizPanel extends EntryPanel {
    private WizardField mTypeFld, mProjLocFld;
    
    /**
     * @param wiz
     */
    public ProjectWizPanel(CreateWizard wiz) {
        super(wiz);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#captureInput() */
    @Override
    protected boolean captureInput() {
        String profile = mTypeFld.getValue();
        System.out.println(profile);
        ProjType projType = ProjType.descriptionOf(profile);
        wizard().tknGen().getTokens().put(Tkn.PROFILE, projType.toString());
        File projRoot = null;
        switch (projType) {
            case ojc: {
                projRoot = new File(new File(
                        System.getProperty(CDK.OJC_ROOT)), "ojc-core");
                break;
            }
            case contrib: {
                projRoot = new File(System.getProperty(CDK.OJC_ROOT));                            
                break;
            }
            case external: {
                String path = mProjLocFld.getValue();
                File root = null;
                if (!Util.isEmpty(path)) {
                    root = new File(path);
                    if (root.exists()) {
                        projRoot = root;
                        mProjLocFld.setValue(projRoot.getAbsolutePath());
                    }
                    else {
                        JOptionPane.showMessageDialog(null, 
                                "Please select an existing location for project to be created!", 
                                "Project Location Not Found", JOptionPane.WARNING_MESSAGE);
                        return false;
                    }
                }
                else {
                    JOptionPane.showMessageDialog(null, 
                            "Please select a project location using the Browse button!", 
                            "Missing Project Location", JOptionPane.WARNING_MESSAGE);
                    return false;
                }
                break;
            }
        }
        
        if (projRoot == null) {
            JOptionPane.showMessageDialog(wizard(), 
                    projType.getDescription() +" projects are not yet supported.", 
                    "Unsupported Project Type!", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        else {
            // override project root based on profile and set on TknGen
            wizard().tknGen().getTokens().put(Tkn.PROJ_ROOT, mProjLocFld.getValue());
        }
        return true;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#createEntryPanel() */
    @Override
    protected JPanel createEntryPanel() {
        JPanel pnl = new JPanel(new GridLayout(2, 1, 5, 10));
        mTypeFld = new WizardField(wizard(), Tkn.PROFILE, "Project Type:", 
                "<html><b>OJC Core</b> components are developed by Sun and form the core of the OpenESB product.<br>"
                +"<b>Partner Contributions</b> are components built by Sun's partners and are part<br>"
                +"of the open-jbi-components project.<br><b>Independent projects</b> are external to,<br> "
                +"and can be built without, the open-jbi-components project.</html>",
                Text.radio, new String[] { ProjType.ojc.getDescription(),
                                           ProjType.contrib.getDescription(),
                                           ProjType.external.getDescription() });
        mTypeFld.addListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (e.getSource() instanceof AbstractButton) {
                    String profile = ((AbstractButton) e.getSource()).getText();
                    ProjType projType = ProjType.descriptionOf(profile);
                    File projRoot = null;
                    String ojc = System.getProperty(CDK.OJC_ROOT);
                    switch (projType) {
                        case ojc: {
                            ojc = acquireOjcRoot(ojc);
                            projRoot = (ojc == null) ? null : new File(new File(
                                    ojc), "ojc-core");
                            mProjLocFld.setValue(projRoot == null
                                    ? "" : projRoot.getAbsolutePath());
                            mProjLocFld.setEnabled(false);
                            break;
                        }
                        case contrib: {
                            ojc = acquireOjcRoot(ojc);
                            projRoot = (ojc == null) 
                                    ? null : new File(ojc);
                            mProjLocFld.setValue(projRoot == null
                                    ? "" : projRoot.getAbsolutePath());
                            mProjLocFld.setEnabled(false);
                            break;
                        }
                        case external: {
                            mProjLocFld.setValue("");
                            mProjLocFld.setEnabled(true);
                            break;
                        }
                    }
                }
            }
        });
        pnl.add(mTypeFld);
        mProjLocFld = new WizardField(wizard(), Tkn.PROJ_ROOT, "Project Location:", 
                "The location of the project, where the project root will be created.\n"+
                "(NOTE: Do not create a directory for the project...)",
                Text.file);
        pnl.add(mProjLocFld);
        mProjLocFld.setEnabled(false);
        return pnl;
    }

    private String acquireOjcRoot(String ojc) {
        if (ojc == null || ojc.contains("$")) {
            // query user
            JFileChooser jfc = new JFileChooser(
                    new File(System.getProperty("CDK_HOME")));
            jfc.setApproveButtonText("Select");
            jfc.setApproveButtonMnemonic('S');
            jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            jfc.setDialogTitle("Please Select OJC Root Directory...");
            
            // show dialog until cancelled or user selects .jbic or top-level POM
            int opt = jfc.showOpenDialog(null);
            if (opt == JFileChooser.APPROVE_OPTION) {
                String root = jfc.getSelectedFile().getAbsolutePath();
                System.setProperty("JV_SRCROOT", root);
                return root;
            }
            else {
                JOptionPane.showMessageDialog(null, 
                        "CDK Wizard cannot continue without environment property JV_SRCROOT", 
                        "Missing OJC Root", JOptionPane.WARNING_MESSAGE);
                return null;
            }
        }
        return ojc;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.wizard.EntryPanel#giveFocus() */
    @Override
    protected void giveFocus() {
        mProjLocFld.giveFocus();
    }

    @Override
    protected void updateInstructions(StyledDocument doc) throws BadLocationException {
        doc.insertString(0, "OJC Core", instructions().getStyle(Boolean.TRUE));
        doc.insertString(doc.getLength(), 
                " components are developed by Sun and form the core of the OpenESB product.\n",
                null);
        doc.insertString(doc.getLength(), "Partner Contributions", instructions().getStyle(Boolean.TRUE));
        doc.insertString(doc.getLength(), 
                " are components built by Sun's partners and are part of the open-jbi-components project.\n", 
                null);
        doc.insertString(doc.getLength(), "Independent Projects", instructions().getStyle(Boolean.TRUE));
        doc.insertString(doc.getLength(), 
                " are external to, and can be built without, the open-jbi-components project.", 
                null);
    }
}
