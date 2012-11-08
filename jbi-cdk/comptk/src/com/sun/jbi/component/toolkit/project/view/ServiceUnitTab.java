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
 * @(#)ServiceUnitTab.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.model.Expr.ProjectExpr;
import com.sun.jbi.component.toolkit.project.services.ServicesBuilder;
import com.sun.jbi.component.toolkit.project.util.Build;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.util.XPathElement.NS;
import com.sun.jbi.component.toolkit.project.view.App.Status;
import com.sun.jbi.component.toolkit.project.view.input.Crud;
import com.sun.jbi.component.toolkit.project.view.input.FileInput;
import com.sun.jbi.component.toolkit.project.view.input.StringInput;

/**
 * 
 * @author Kevan Simpson
 */
public class ServiceUnitTab extends BasePanel {
    private DescriptionList mBldrsList, mUnitsList;
    
    /**
     * @param app
     * @param params
     */
    public ServiceUnitTab(App app) {
        super(app);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
    @Override
    public boolean updateValues() {
        if (super.updateValues()) {
            updateFields(getApp().getProject().getServiceConfig());
            return true;
        }
        
        return false;
    }

    protected void createServiceUnit(boolean preview) {
        try {
            String bldr = mBldrsList.getText(), unit = mUnitsList.getText(),
                   loc = getApp().getProject().getServiceConfig().getUnitLocation(unit);
            if (Util.isEmpty(bldr) || Util.isEmpty(unit)) {
                JOptionPane.showMessageDialog(null, 
                        "Please select a Service Unit Builder and Definition!", 
                        "Missing Required Selection!", JOptionPane.WARNING_MESSAGE);
                return;
            }
            else if (Util.isEmpty(loc)) {
                JOptionPane.showMessageDialog(null, 
                        "The Service Unit's Location is undefined!", 
                        "Missing Service Unit Location!", JOptionPane.WARNING_MESSAGE);
                return;
                
            }
            
            if (preview) {
                String xml = Build.getBuild().buildServicesDescriptor(
                        getApp().getProject(), bldr, unit);
                String[] msgs = Util.tokenize(xml, "\n");
                MessagePanel.showMessageDialog(
                        unit +" JBI Descriptor", Status.good, msgs);
            }
            else {
                try {
                    String[] out = Build.getBuild()
                            .generateSUArtifact(getApp(), bldr, unit);
                    MessagePanel.showMessageDialog(
                            "Create Service Unit", Status.good, out);
                    getApp().showMessages(Status.good, 
                            "Service Unit "+ unit +" created successfully!");
                }
                catch (Exception e) {
                    getApp().showMessages(Status.error, 
                            "Service Unit "+ unit +" creation failed: "+ e.getMessage());
                }
            }
        }
        catch (Exception e) {
            getApp().handleError(e, 
                    "Failed to generate service unit artifact: {0}", 
                    e.getMessage());
        }
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        setLayout(new GridLayout(3, 1));
        setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), "Service Units"));
        mBldrsList = new DescriptionList(getApp(), ProjectExpr.builders,
                "SU Builders", "Defined Service Unit Builders");
        addField(mBldrsList);

        JPanel pnlBldrs = createListPanel(mBldrsList);
        Crud crud = new Crud(
                getApp(), mBldrsList, "Service Unit Builder", 
                NS.CDK.newQName("builder"), "cdk:unit[1]",
                new StringInput("Service Unit Builder", "name"),
                new StringInput("Service Unit Builder", "type", "@type")) {
                    /** @see com.sun.jbi.component.toolkit.project.view.input.Crud#isReadOnly(java.lang.String) */
                    @Override
                    protected boolean isReadOnly(String text) {
                        XPathElement xpel = mBldrsList.getSelected();
                        if (xpel != null) {
                            String oldType = xpel.getString("@type");
                            if (oldType != null && 
                                oldType.startsWith(ServicesBuilder.class.getPackage().getName())) {
                                // pre-defined builder...cannot remove
                                return true;
                            }
                        }

                        return super.isReadOnly(text);
                    }
        };
        mBldrsList.setCrud(crud);
        add(pnlBldrs);
        
        mUnitsList = new DescriptionList(getApp(), ProjectExpr.units,
                "Service Units", "Service Unit Definitions");
        mUnitsList.setCrud(NS.CDK.newQName("unit"), null, 
                new StringInput("Service Unit Definition", "name"),
                new FileInput("Service Unit Root", "location", "@loc"));
        addField(mUnitsList);
        add(createListPanel(mUnitsList));
        
        JPanel pnlBtns = new JPanel(new FlowLayout(FlowLayout.CENTER));
        pnlBtns.add(new JButton(new AbstractAction("Create") {
            public void actionPerformed(ActionEvent e) {
                createServiceUnit(false);
            }
        }));
        pnlBtns.add(new JButton(new AbstractAction("Preview") {
            public void actionPerformed(ActionEvent e) {
                createServiceUnit(true);
            }
        }));
        pnlBtns.add(new JButton(new AbstractAction("Tutorial") {
            public void actionPerformed(ActionEvent e) {
                // TODO i18n and move elsewhere?
                MessagePanel.showMessageDialog(
                        "CDK Service Unit Tutorial", Status.good, 
                        "How to Build a Service Unit using CDK",
                        "------------------------------------------------------------",
                        "1.  Select a Service Unit Builder to generate the",
                        "    JBI descriptor.",
                        "2.  Create and select a Service Unit Definition.",
                        "3.  Create SU Artifact or Preview JBI Descriptor");
            }
        }));
        add(pnlBtns);
    }

    private JPanel createListPanel(DescriptionList list) {
        JPanel pnl = new JPanel(new BorderLayout());
        pnl.add(list, BorderLayout.CENTER);
        return pnl;
    }
}
