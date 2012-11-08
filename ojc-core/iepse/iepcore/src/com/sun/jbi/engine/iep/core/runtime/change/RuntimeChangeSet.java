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

package com.sun.jbi.engine.iep.core.runtime.change;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author rdwivedi
 */
public class RuntimeChangeSet {
    
    private List<RuntimeChangeObject> mChanges;
    public RuntimeChangeSet() {
        mChanges = new ArrayList<RuntimeChangeObject>();
    }
    
    public void addAChange(RuntimeChangeObject change){
        mChanges.add(change);
    }
    public List<RuntimeChangeObject> getChangeList() {
        return Collections.unmodifiableList(mChanges);
    }
    public void clear() {
        mChanges.clear();
    }
    public boolean hasChanges() {
        return !mChanges.isEmpty();
    }
}
