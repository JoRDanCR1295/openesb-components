package org.glassfish.openesb.pojose.core.anno.meta;
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
 * @(#)OperationMetadata.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
import org.glassfish.openesb.pojose.api.annotation.Operation;

/*
 * @author sgenipud
 * @author gpatil
 */

public class OperationMetadata extends BaseMethodMetaData {
    private Operation operation;
    private String mOperationName = null;
    
    public OperationMetadata() {
    }
    
    public String getOperationName() {
        return mOperationName;
    }
    
    public void setOperationName(String opn) {
        mOperationName = opn;
    }
        
    public Operation getOperation() {
        return operation;
    }

    public void setOperation(Operation operation) {
        this.operation = operation;
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 97 * hash + (this.mOperationName != null ? this.mOperationName.hashCode() : 0);
        hash = 97 * hash + (this.mActualMethodName != null ? this.mActualMethodName.hashCode() : 0);
        hash = 97 * hash + (this.methodParameterType != null ? this.methodParameterType.hashCode() : 0);
        hash = 97 * hash + (this.methodReturnType != null ? this.methodReturnType.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object ob) {
        if (ob instanceof OperationMetadata) {
            OperationMetadata opM = (OperationMetadata)ob;
            if ( opM.getMethodName() == null && opM.getOperationName().equals(this.getOperationName())) {
                return true;
            } else if ( this.getMethodName() == null && opM.getOperationName().equals(this.getOperationName())) {
                return true;
            } else if (opM.getMethodName().equals(this.getMethodName()) &&
                opM.getOperationName().equals(this.getOperationName())) {
                return true;
            } else
            return false;
        }

        if ( ob instanceof String) {
            String opnName = (String)ob;
            if (this.getOperationName().equals(opnName)) {
                return true;
            }
        }
        return false;
    }
}
