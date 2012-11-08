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

import java.awt.GridLayout;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
import com.sun.jbi.component.toolkit.project.model.Expr.JbiExpr;
import com.sun.jbi.component.toolkit.project.util.XPathElement.NS;
import com.sun.jbi.component.toolkit.project.view.input.StringInput;

/**
 * 
 * @author Kevan Simpson
 */
public class ClasspathTab extends BasePanel {
    private DescriptionList mCompPath, mBootPath, mSharedLibs;

    /**
     * 
     */
    public ClasspathTab(App app) {
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
        JPanel pnlComp = new JPanel(new GridLayout(3, 1, 0, 5));
        pnlComp.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), "component"));

        mCompPath = new DescriptionList(getApp(), JbiExpr.component_path_element, 
                "component-class-path", "The jars in the runtime component classpath.");
        mCompPath.setCrud(NS.JBI.newQName("path-element"), null, 
                new StringInput("Runtime CP Jar", "relative path"));
        addField(mCompPath);
        pnlComp.add(mCompPath);
        
        mBootPath = new DescriptionList(getApp(), JbiExpr.bootstrap_path_element, 
                "bootstrap-class-path", "The jars in the bootstrap/installation classpath.");
        mBootPath.setCrud(NS.JBI.newQName("path-element"), null, 
                new StringInput("Bootstrap CP Jar", "relative path"));
        addField(mBootPath);
        pnlComp.add(mBootPath);
        
        mSharedLibs = new DescriptionList(getApp(), JbiExpr.shared_libraries,
                "shared-library", "List of shared libraries ONLY available in runtime component classpath.");
        mSharedLibs.setCrud(NS.JBI.newQName("shared-library"), 
                            "(cfg:Configuration | log:Logging)[1]",
                            new StringInput("Shared Library", "name"));
        addField(mSharedLibs);
        pnlComp.add(mSharedLibs);
        pnlJbi.add(pnlComp);
        add(pnlJbi);
    }
}
