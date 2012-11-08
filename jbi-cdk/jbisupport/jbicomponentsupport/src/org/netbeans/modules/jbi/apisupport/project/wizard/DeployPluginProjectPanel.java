/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */


package org.netbeans.modules.jbi.apisupport.project.wizard;

import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardPanel;
import org.netbeans.modules.jbi.apisupport.common.wizard.AbstractWizardVisualPanel;
import org.openide.util.HelpCtx;

/**
 * Panel just asking for basic info.
 * @author chikkala
 */
public final class DeployPluginProjectPanel extends AbstractWizardPanel {
    private String mCompType;
    /** Create the wizard panel descriptor. */
    public DeployPluginProjectPanel(String compType) {
        super();
        this.mCompType = compType;
    }
    
    @Override
    public HelpCtx getHelp() {
        return HelpCtx.DEFAULT_HELP;
    }
    
    @Override
    public boolean isFinishPanel() {
        return true;
    }
    
    protected AbstractWizardVisualPanel createVisualPanel() {
        return new DeployPluginProjectVisualPanel(this, this.mCompType);
    }
    
}
