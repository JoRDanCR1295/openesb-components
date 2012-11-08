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
 * @(#)AppEvent.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.event;

import java.util.EventObject;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.view.DescriptionField;

/**
 * 
 * @author Kevan Simpson
 */
public class AppEvent extends EventObject {
    public enum Type {
        commit,
        display_source,
        field_entered,
        load_project,
//        radio_selected,
        save_complete,
        text_changed
    }

    private Type mType;
    
    /**
     * @param source
     */
    public AppEvent(Type type, Object source) {
        super(source);
        mType = type;
    }

    public DescriptionField getField() {
        return (DescriptionField) getSource();
    }
    
    public Project getProject() {
        return (Project) getSource();
    }
    
    public Type getType() {
        return mType;
    }
}
