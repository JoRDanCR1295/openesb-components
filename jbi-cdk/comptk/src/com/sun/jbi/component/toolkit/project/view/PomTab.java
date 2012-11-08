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
 * @(#)PomTab.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.GridLayout;
import javax.swing.BorderFactory;
import javax.swing.JTabbedPane;
import javax.swing.border.EtchedBorder;
import com.sun.jbi.component.toolkit.project.model.Pom.Module;

/**
 * 
 * @author Kevan Simpson
 */
public class PomTab extends BasePanel {
    private JTabbedPane mModuleTabs;
    
    /**
     * @param app
     * @param params
     */
    public PomTab(App app, Object... params) {
        super(app, params);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
    @Override
    protected boolean updateValues() {
        // TODO Look for added/removed modules to update tabs?
        return super.updateValues();
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        setLayout(new GridLayout());
        setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), 
                "Modules"));
        
        mModuleTabs = new JTabbedPane();
        for (Module mod : Module.values()) {
            addModule(mod.toString());
        }
        for (String name : getApp().getProject().getModuleNames()) {
            boolean customModule = false;
            try { Module.valueOf(name); }
            catch (IllegalArgumentException iae) { customModule = true; }
            
            if (customModule) {
                addModule(name);
            }
        }
        add(mModuleTabs);
    }

    private void addModule(String pomName) {
        if (getApp().getProject().getModule(pomName) != null) {
            PomView view = new PomView(getApp(), pomName);
            mModuleTabs.addTab(pomName, view);
        }
    }
}
