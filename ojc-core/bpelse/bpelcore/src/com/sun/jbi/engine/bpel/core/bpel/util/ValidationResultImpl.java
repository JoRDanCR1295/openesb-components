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
package com.sun.jbi.engine.bpel.core.bpel.util;

import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Vitaly Bychkov
 */
public class ValidationResultImpl<T> implements ValidationResult<T> {

    private T source;
    private Object value;
    private Collection<Object> errors;

    public ValidationResultImpl(T source, Object value, Collection<Object> errors) {
        this.source = source;
        this.value = value;
        this.errors = errors;
    }

    public T getSource() {
        return source;
    }

    public Object getValue() {
        return value;
    }

    public Collection<Object> getErrors() {
        return errors == null ? Collections.EMPTY_LIST: errors;
    }
}
