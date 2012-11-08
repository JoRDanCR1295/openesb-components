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
 * @(#)ComponentTab.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
import com.sun.jbi.component.toolkit.project.model.Expr.JbiExpr;
import com.sun.jbi.component.toolkit.project.view.DescriptionField.Text;

/**
 * 
 * @author Kevan Simpson
 */
public class ComponentTab extends BasePanel {
    private DescriptionField  mNameFld, mDescFld;    // jbi/component/identification
    private DescriptionField mComponentClassFld, mComponentDescFld, mBootstrapClassFld;
    private DescriptionField mTypeRadios;

    /**
     * 
     */
    public ComponentTab(App app) {
        super(app);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
    @Override
    protected boolean updateValues() {
        if (super.updateValues()) {
            updateFields(getApp().getProject().getComponent());
            return true;
        }
        
        return false;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    protected void init(Object... params) {
        setLayout(new GridLayout());
        JPanel pnlJbi = new JPanel(new GridLayout());
        pnlJbi.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), "jbi"));
        JPanel pnlComp = new JPanel(new GridLayout(2, 1, 0, 5));
        pnlComp.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), "component"));
        
        JPanel pnlTypeId = new JPanel(new BorderLayout());
        mTypeRadios = new DescriptionField(getApp(), JbiExpr.type,
                "@type", "The type of JBI component", 
                new String[] { "service-engine", "binding-component" });
        addField(mTypeRadios);
        pnlTypeId.add(mTypeRadios, BorderLayout.NORTH);
        JPanel pnlId = new JPanel(new GridLayout(2, 1, 0, 0));
        pnlId.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), "identification"));
        mNameFld = new DescriptionField(getApp(), JbiExpr.name, "name",
                "The fully-qualified name of the component");
        addField(mNameFld);
        pnlId.add(mNameFld);
        mDescFld = new DescriptionField(getApp(), JbiExpr.description,
                "description", "An extended description of the component", Text.area);
        addField(mDescFld);
        pnlId.add(mDescFld);
        pnlTypeId.add(pnlId, BorderLayout.CENTER);
        
        JPanel pnlCP = new JPanel(new BorderLayout());
        JPanel pnlClasses = new JPanel(new GridLayout(3, 1, 0, 0));
        pnlClasses.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEmptyBorder(1, 1, 1, 1), ""));
        mComponentClassFld = new DescriptionField(getApp(), 
                JbiExpr.manager_class, 
                "component-class-name", 
                "The fully-qualified classname of the component implementation");
        addField(mComponentClassFld);
        pnlClasses.add(mComponentClassFld);
        mComponentDescFld = new DescriptionField(getApp(),
                JbiExpr.manager_description,
                "component-class-name/@description", 
                "A description of the component implementation");
        addField(mComponentDescFld);
        pnlClasses.add(mComponentDescFld);
        mBootstrapClassFld = new DescriptionField(getApp(),
                JbiExpr.bootstrap_class,
                "bootstrap-class-name", 
                "The fully-qualified classname of the bootstrap implementation");
        addField(mBootstrapClassFld);
        pnlClasses.add(mBootstrapClassFld);
        pnlCP.add(pnlClasses, BorderLayout.NORTH);
        
        pnlComp.add(pnlTypeId);
        pnlComp.add(pnlCP);
        
        pnlJbi.add(pnlComp);
        add(pnlJbi);
    }
}
