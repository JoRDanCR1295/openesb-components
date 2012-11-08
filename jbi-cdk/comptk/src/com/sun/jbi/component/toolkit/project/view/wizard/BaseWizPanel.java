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
 * @(#)BaseWizPanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import com.sun.jbi.component.toolkit.project.view.BasePanel;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class BaseWizPanel extends BasePanel {
    private CreateWizard mWizard;
    
    /**
     * @param app
     * @param params
     */
    protected BaseWizPanel(CreateWizard wiz) {
        super(null, wiz);
    }
    
    protected BaseWizPanel(Object... params) {
        super(null, params);
    }

    public abstract void showStep(Wiz wiz);
    
    protected int current() {
        return wizard().current();
    }
    
    protected void giveFocus() {
    }
    
    protected int previous() {
        return wizard().previous();
    }
    
    protected Wiz[] steps() {
        return wizard().steps();
    }
    
    protected CreateWizard wizard() {
        return mWizard;
    }

    protected void setWizard(CreateWizard wiz) {
        mWizard = wiz;
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        setWizard((CreateWizard) params[0]);
    }
}
