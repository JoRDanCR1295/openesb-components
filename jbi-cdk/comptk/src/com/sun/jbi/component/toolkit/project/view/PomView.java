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
 * @(#)PomView.java
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
import com.sun.jbi.component.toolkit.project.model.Pom;
import com.sun.jbi.component.toolkit.project.model.Expr.PomExpr;
import com.sun.jbi.component.toolkit.project.view.DescriptionField.Text;

/**
 * 
 * @author Kevan Simpson
 */
public class PomView extends BasePanel {
    private DescriptionField mParentId, mParentGroupId;
    private DescriptionField mParentVersion, mParentPath;
    private DescriptionField mGroupIdFld, mArtifactIdFld, mNameFld;
    private DescriptionField mVersionFld, mDescFld;

    private String mPomName;    // to load from project
    
    /**
     * @param app
     * @param params
     */
    public PomView(App app, String pomName) {
        super(app, pomName);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#updateValues() */
    @Override
    protected boolean updateValues() {
        if (super.updateValues()) {
            Pom pom = getApp().getProject().getModule(mPomName);
            if (pom != null) {
                updateFields(pom);
                return true;
            }
        }
        
        return false;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.BasePanel#init(java.lang.Object[]) */
    @Override
    protected void init(Object... params) {
        mPomName = String.valueOf(params[0]);
        
        setLayout(new BorderLayout(0, 5));
        setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), 
                "project"));
        JPanel pnlParent = new JPanel(new GridLayout(4, 1));
        pnlParent.setBorder(BorderFactory.createTitledBorder(
                BorderFactory.createEtchedBorder(EtchedBorder.LOWERED), 
                "parent"));
        mParentId = new DescriptionField(getApp(), PomExpr.parent_id,
                "artifactId", "The parent POM's artifactId", Text.label);
        addField(mParentId);
        pnlParent.add(mParentId);
        mParentGroupId = new DescriptionField(getApp(), PomExpr.parent_group_id,
                "groupId", "The parent POM's groupId", Text.label);
        addField(mParentGroupId);
        pnlParent.add(mParentGroupId);
        mParentVersion = new DescriptionField(getApp(), PomExpr.parent_version,
                "version", "The parent POM's version", Text.label);
        addField(mParentVersion);
        pnlParent.add(mParentVersion);
        mParentPath = new DescriptionField(getApp(), PomExpr.parent_path,
                "relativePath", "The parent POM's location relative to this POM", Text.label);
        addField(mParentPath);
        pnlParent.add(mParentPath);
        add(pnlParent, BorderLayout.NORTH);
        
        JPanel pnlCtr = new JPanel(new GridLayout(5, 1));
        mGroupIdFld = new DescriptionField(getApp(), PomExpr.group_id,
                "groupId", "The POM's groupId", Text.label);
        addField(mGroupIdFld);
        pnlCtr.add(mGroupIdFld);
        mArtifactIdFld = new DescriptionField(getApp(), PomExpr.artifact_id,
                "artifactId", "The POM's artifactId", Text.label);
        addField(mArtifactIdFld);
        pnlCtr.add(mArtifactIdFld);
        mNameFld = new DescriptionField(getApp(), PomExpr.name,
                "name", "The POM's name", Text.label);
        addField(mNameFld);
        pnlCtr.add(mNameFld);
        mVersionFld = new DescriptionField(getApp(), PomExpr.version,
                "version", "The POM's version", Text.label);
        addField(mVersionFld);
        pnlCtr.add(mVersionFld);
        mDescFld = new DescriptionField(getApp(), PomExpr.description,
                "description", "The POM's description", Text.label);
        addField(mDescFld);
        pnlCtr.add(mDescFld);
        add(pnlCtr, BorderLayout.CENTER);
    }

}
