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
 * @(#)Description.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.util;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Check out the meta-annotations in java.lang.annotation Retention specifies
 * whether the annotation is used only by tools (like the compiler) that read
 * source code; or by tools that read class files; or by code that uses
 * reflection. You might want to define some annotations that work in
 * conjunction with the apt tool, in which case all three retention values could
 * be interesting. Usually, though, you will want RetentionPolicy.RUNTIME.
 * 
 * Target specifies a list of places in the Java language syntax where the
 * annotation can be used. This one is only appropriate as an annotation on
 * parameters, so that's what you say.
 * 
 * @author narayanaa
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.CONSTRUCTOR, ElementType.METHOD, ElementType.PARAMETER,
        ElementType.TYPE })
public @interface Description {
    
    /**
     * By the way, it's important that the name of the method in the
     * 
     * @Description annotation be value and not anything else. If it were called
     *              name, say, you would have to write
     * @Description(name="n") rather than just
     * @Description("n").
     * 
     * @return description string
     */
    String value();
}
