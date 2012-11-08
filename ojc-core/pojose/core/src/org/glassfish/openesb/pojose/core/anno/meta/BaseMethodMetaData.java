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

package org.glassfish.openesb.pojose.core.anno.meta;

import java.lang.reflect.Method;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyMethod;

/**
 *
 * @author gpatil
 */
public class BaseMethodMetaData {
    protected Method method;
    protected String mActualMethodName = null;
    protected ProxyMethod.ParamType methodParameterType = null;
    protected ProxyMethod.ParamType methodReturnType = null;

    public ProxyMethod.ParamType getMethodReturnType() {
        return this.methodReturnType;
    }

    public Method getMethod() {
        return method;
    }

    public void setMethod(Method method) {
        this.method = method;
    }

    public String getMethodName() {
        return mActualMethodName;
    }

    public void setMethodName(String methodName) {
        mActualMethodName = methodName;
    }
    
    public void setMethodParameterType(ProxyMethod.ParamType type ) {
        this.methodParameterType = type;
    }

    public ProxyMethod.ParamType getMethodParameterType() {
        return this.methodParameterType;
    }

    public void setMethodReturnType(ProxyMethod.ParamType type) {
        this.methodReturnType  = type;
    }


    @Override
    public int hashCode() {
        int hash = 3;
        hash = 67 * hash + (this.method != null ? this.method.hashCode() : 0);
        hash = 67 * hash + (this.mActualMethodName != null ? this.mActualMethodName.hashCode() : 0);
        hash = 67 * hash + (this.methodParameterType != null ? this.methodParameterType.hashCode() : 0);
        hash = 67 * hash + (this.methodReturnType != null ? this.methodReturnType.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final BaseMethodMetaData other = (BaseMethodMetaData) obj;
        if (this.method != other.method && (this.method == null || !this.method.equals(other.method))) {
            return false;
        }
        if ((this.mActualMethodName == null) ? (other.mActualMethodName != null) : !this.mActualMethodName.equals(other.mActualMethodName)) {
            return false;
        }
        if (this.methodParameterType != other.methodParameterType) {
            return false;
        }
        if (this.methodReturnType != other.methodReturnType) {
            return false;
        }
        return true;
    }

    
}
