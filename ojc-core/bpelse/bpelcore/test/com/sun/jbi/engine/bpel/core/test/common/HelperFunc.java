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
 * @(#)HelperFunc.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.common;

import java.util.Collection;
import java.util.Iterator;

import com.sun.bpel.model.If;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;

/**
 * @author Sun Microsystems
 */
public final class HelperFunc {
    private static RActivity getActivityByName(Object obj, String actName) {
        if (obj instanceof RActivity) {
            RActivity act = (RActivity) obj;

            if ((act.getName() != null) && act.getName().equals(actName)) {
                return (RActivity) obj;
            }
        }
        return null;
    }

    public static RActivity getActivity(RActivityHolder root, String actName) {
        RActivity retVal = getActivityByName(root, actName);

        if (retVal != null) {
            return retVal;
        }

        for (RActivity child = root.getChildActivity(); child != null;
                child = child.getNextActivity()) {
            if (child instanceof RActivityHolder) {
                retVal = getActivity((RActivityHolder) child, actName);
            } else if (child instanceof RActivity) {
                retVal = getActivityByName(child, actName);
            }

            if ((retVal == null) && child instanceof Pick) {
                retVal = getActivityWithinPick((Pick) child, actName);
            }

            if ((retVal == null) && child instanceof If) {
                retVal = getActivityWithinElseIfs((If) child, actName);
            }
            
            if (retVal != null) {
                return retVal;
            }
        }

        return null;
    }

    public static Object[] getActivityWithBranchId(RActivityHolder root,
        String actName, long branchId) {
        for (RActivity child = root.getChildActivity(); child != null;
                child = child.getNextActivity()) {
            Object[] obj = null;

            if (child instanceof RActivityHolder) {
                obj = getActivityWithBranchId((RActivityHolder) child, actName,
                        child.getUniqueId());
            }

            if (obj != null) {
                return obj;
            }

            if (child instanceof RActivity) {
                RActivity act = (RActivity) child;

                if ((act.getName() != null) && act.getName().equals(actName)) {
                    return new Object[] { child, new Long(branchId) };
                }
            }
        }

        return null;
    }

    private static RActivity getActivityWithinPick(Pick pick, String actName) {
        RActivity retVal = null;

        for (int i = 0, onAlrmSize = pick.getOnAlarmSize(); i < onAlrmSize;
                i++) {
            RActivityHolder onAlrm = (RActivityHolder) pick.getOnAlarm(i);
            retVal = getActivity(onAlrm, actName);

            if (retVal != null) {
                return retVal;
            }
        }

        for (int j = 0, onMesgSize = pick.getOnMessageSize(); j < onMesgSize;
                j++) {
            RActivityHolder onMesg = (RActivityHolder) pick.getOnMessage(j);
            retVal = getActivity(onMesg, actName);

            if (retVal != null) {
                return retVal;
            }
        }

        return retVal;
    }
    
    public static RActivity getScopeByName(RActivityHolder root, String actName) {
        RActivity retVal = getActivityByName(root, actName);

        if (retVal != null) {
            return retVal;
        }

        for (RActivity child = root.getChildActivity(); child != null;
                child = child.getNextActivity()) {
            if (child instanceof RActivityHolder) {
                retVal = getActivityByName(child, actName);
                if (retVal == null) {
                    retVal = getScopeByName((RActivityHolder) child, actName);
                }
            }
            if ((retVal == null) && child instanceof Pick) {
                retVal = getScopeWithinPick((Pick) child, actName);
            }

            if (retVal != null) {
                return retVal;
            }
        }

        return null;
    }
    private static RActivity getScopeWithinPick(Pick pick, String actName) {
        RActivity retVal = null;

        for (int i = 0, onAlrmSize = pick.getOnAlarmSize(); i < onAlrmSize;
                i++) {
            RActivityHolder onAlrm = (RActivityHolder) pick.getOnAlarm(i);
            retVal = getScopeByName(onAlrm, actName);

            if (retVal != null) {
                return retVal;
            }
        }

        for (int j = 0, onMesgSize = pick.getOnMessageSize(); j < onMesgSize;
                j++) {
            RActivityHolder onMesg = (RActivityHolder) pick.getOnMessage(j);
            retVal = getScopeByName(onMesg, actName);

            if (retVal != null) {
                return retVal;
            }
        }

        return retVal;
    }
    
    private static RActivity getActivityWithinElseIfs(If ifObj, String actName) {
        RActivity retVal = null;
        Collection elseIfs = ifObj.getElseIfs();
        for (Iterator itr = elseIfs.iterator(); itr.hasNext(); ) {
            RActivityHolder elseIf = (RActivityHolder) itr.next();
            retVal = getActivity(elseIf, actName);
            if (retVal != null) {
                return retVal;
            }
        }

        RActivityHolder elseObj = (RActivityHolder) ifObj.getElse();
        retVal = getActivity(elseObj, actName);
        return retVal;
    }
}
