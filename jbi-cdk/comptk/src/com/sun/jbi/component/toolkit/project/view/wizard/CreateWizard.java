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
 * @(#)CreateWizard.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.wizard;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Map;
import javax.swing.JDialog;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.GenerateTokens.Tkn;
import com.sun.jbi.component.toolkit.project.GenerateTokens.TokenGen;
import com.sun.jbi.component.toolkit.project.util.I18n;

/**
 * 
 * @author Kevan Simpson
 */
public class CreateWizard extends JDialog {
    public enum CompType {
        engine("Service Engine"),
        binding("Binding Component");
        
        private String mDesc, mAttr, mSuffix, mEnum, mType;
        
        private CompType(String desc) {
            mDesc = desc;
            mType = this.toString();
            mAttr = desc.replace(" ", "-").toLowerCase();
            int space = desc.indexOf(" ");
            mSuffix = desc.substring(0, 1) + desc.substring(space + 1, space + 2);
            mEnum = desc.replace(" ", "");
        }
        
        /** 
         * Returns the desc.
         * @return the desc. 
         */
        public String getDesc() {
            return mDesc;
        }
        /** 
         * Returns the attr.
         * @return the attr. 
         */
        public String getAttr() {
            return mAttr;
        }
        /** 
         * Returns the suffix.
         * @return the suffix. 
         */
        public String getSuffix() {
            return mSuffix;
        }
        /** 
         * Returns the enum.
         * @return the enum. 
         */
        public String getEnum() {
            return mEnum;
        }
        /** 
         * Returns the type.
         * @return the type. 
         */
        public String getType() {
            return mType;
        }
        
        public static CompType descriptionOf(String desc) {
            if (!Util.isEmpty(desc)) {
                return desc.equals(engine.getDesc()) ? engine : binding;
            }
            return null;
        }
    }
    
    public enum Wiz { 
        start("Component Type"), 
        project("Project Template"), 
        name("Component Project Name");//, 
//        pom("JBIC Maven POMs");
        
        private String mHelp;
        private Wiz(String help) {
            mHelp = help;
        }
        
        public String getHelp() {
            return mHelp;
        }
    }

    private WizardPanel mWizardPnl;
    private HelpPanel mHelpPnl;
    private TraversePanel mTraversePnl;
    
    private TokenGen mTknGen;
    private int mCurrentIndex, mPrevIndex;
    private Wiz[] mSteps;   // cached, just cuz

    /**
     */
    public CreateWizard(TokenGen tkns) {
        super((Frame) null, "JBI Component Creation Wizard", true);
        mTknGen = tkns;
        init();
    }

    public Map<Tkn, String> getTokens() {
        return (current() == Integer.MIN_VALUE) ? null : tknGen().getTokens();
    }
    
    public boolean isCanceled() {
        return current() == Integer.MIN_VALUE;
    }
    
    public void showStep(Wiz wiz) {
        if (mWizardPnl.captureInput()) {
            mTraversePnl.showStep(wiz);
            mHelpPnl.showStep(wiz);
            mWizardPnl.showStep(wiz);
        }
        else {
            move(-1);
//            JOptionPane.showMessageDialog(null, String.valueOf(mTknGen.getTokens()));
        }
    }

    protected void giveFocus() {
        mWizardPnl.giveFocus();
    }
    
    protected void move(int delta) {
        mPrevIndex = mCurrentIndex;
        mCurrentIndex += delta;
        showStep(steps()[current()]);
    }

    protected void traverse(int delta) {
        switch (delta) {
            case Integer.MAX_VALUE: {   // finish
                try {
                    move(0);
                    tknGen().initDefaultTokens();
                }
                catch (Exception e) {
                    System.err.println(I18n.loc(
                            "Failed to initialize default token values: {0}",
                            e.getMessage()));
                    e.printStackTrace();
                    System.exit(1);
                }
                finally {
                    setVisible(false);
                }
                break;
            }
            case Integer.MIN_VALUE: {   // cancel
                cancel(true);
                break;
            }
            case -1: {   // back
                if (current() > 0) {
                    move(-1);
                }
                break;
            }
            case 1: {   // next
                if (current() < (steps().length - 1)) {
                    move(1);
                }
                break;
            }
        }
    }

    void cancel(boolean hide) {
        mCurrentIndex = Integer.MIN_VALUE;
        if (hide) {
            setVisible(false);
        }
    }
    
    int current() {
        return mCurrentIndex;
    }

    int previous() {
        return mPrevIndex;
    }

    Wiz[] steps() {
        return mSteps;
    }

    TokenGen tknGen() {
        return mTknGen;
    }
    
    protected void init() {
        mCurrentIndex = 0;
        mSteps = Wiz.values();

        Container pane = getContentPane();
        pane.setLayout(new BorderLayout(5, 5));
        mHelpPnl = new HelpPanel(this, false);
        pane.add(mHelpPnl, BorderLayout.WEST);
        mWizardPnl = new WizardPanel(this);
        pane.add(mWizardPnl, BorderLayout.CENTER);
        mTraversePnl = new TraversePanel(this);
        pane.add(mTraversePnl, BorderLayout.SOUTH);
        
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                cancel(false);
            }
        });
        pack();
        setLocationRelativeTo(null);
    }
}
