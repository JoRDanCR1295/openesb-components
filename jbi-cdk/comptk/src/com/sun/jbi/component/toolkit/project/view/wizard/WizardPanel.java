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

import java.awt.CardLayout;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.GenerateTokens.TokenGen;
import com.sun.jbi.component.toolkit.project.scrub.Profile.ProjType;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.CompType;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz;

/**
 * 
 * @author Kevan Simpson
 */
public class WizardPanel extends BaseWizPanel {
    private CardLayout mLayout;
    private Map<Wiz, EntryPanel> mEntryPnls;
    
    /**
     * @param wiz
     */
    public WizardPanel(CreateWizard wiz) {
        super(wiz);
    }

    public boolean captureInput() {
        // capture tokens, if user clicked Next
        boolean next = current() >= previous();
        if (next) {
            return mEntryPnls.get(steps()[previous()]).captureInput();
        }
        
        return true;
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.wizard.BaseWizPanel#showStep(com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.Wiz) */
    public void showStep(Wiz wiz) {
        System.out.println("WizardPanel.showStep(): "+ wiz +" ("+ previous() +","+ current() +")");
        System.out.println(wizard().getTokens());
        // capture tokens, if user clicked Next
        boolean next = current() >= previous();
        if (next) {
            Wiz prev = steps()[previous()];
            mEntryPnls.get(steps()[current()]).giveFocus();
            TokenGen tknGen = wizard().tknGen();
            switch (prev) {
                case start: {   // will only come here on user clicking Back btn...
                    tknGen.setCompType(CompType.descriptionOf(
                            tknGen.getTokens().get(Tkn.COMP_TYPE_DESC)));
                    break;
                }
                case project: {
                    String profile = tknGen.getTokens().get(Tkn.PROFILE);
                    ProjType projType = ProjType.valueOf(profile);
                    File projRoot = null;
                    switch (projType) {
                        case ojc: {
                            projRoot = new File(new File(
                                    System.getProperty("JV_SRCROOT")), "ojc-core");
                            break;
                        }
                        case contrib: {
                            projRoot = new File(System.getProperty("JV_SRCROOT"));                            
                            break;
                        }
                        case external: {
                            projRoot = new File(tknGen.getTokens().get(Tkn.PROJ_ROOT));
                            break;
                        }
                    }
                    // override project root based on profile and set on TknGen
                    tknGen.setProjectRoot(projRoot);
                    break;
                }
                case name: {
                    tknGen.setProjectName(tknGen.getTokens().get(Tkn.PROJ_NAME));
                    break;
                }
//                case pom: {
//                    
//                    break;
//                }
            }
        }
        mLayout.show(this, wiz.toString());
//        mEntryPnls.get(wiz.toString()).giveFocus();
    }

    @Override
    protected void init(Object... params) {
        super.init(params); // sets CreateWizard field
        mLayout = new CardLayout();
        setLayout(mLayout);

        EntryPanel pnl = null;
        mEntryPnls = new HashMap<Wiz, EntryPanel>();
        for (Wiz wiz : Wiz.values()) {
            switch (wiz) {
                case start: {
                    pnl = new CompTypePanel(wizard());
                    pnl.giveFocus();
                    break;
                }
                case project: {
                    pnl = new ProjectWizPanel(wizard());
                    break;
                }
                case name: {
                    pnl = new ComponentWizPanel(wizard());
                    break;
                }
//                case pom: {
//                    pnl = new PomWizPanel(wizard());
//                    break;
//                }
            }

            if (pnl != null) {
                mEntryPnls.put(wiz, pnl);
                add(pnl, wiz.toString());
            }
        }
    }
}
