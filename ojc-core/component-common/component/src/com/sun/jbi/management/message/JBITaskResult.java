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
 * @(#)JBITaskResult.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import com.sun.jbi.util.JBIUtils;

/**
 * This class embodies the <code>jbi-task-result</code> element of a JBI
 * management message.
 *
 * @author Sun Microsystems
 *
 */
public class JBITaskResult extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(JBITaskResult.class.getName());
    /**
     * The element name: <code>jbi-task-result</code>.
     */
    public static final String ELEMENT_NAME = "jbi-task-result";

    /**
     * The framework task result (<code>frmwk-task-result</code>).
     */
    private FrameworkTaskResult frameworkTaskResult;

    /**
     * A list of zero or more component task results
     * (<code>component-task-result</code>).
     */
    private List componentTaskResults;

    /**
     * Get a string serialization of the task result message conveyed by
     * this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing this task result.
     */
    protected String buildString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");
        buffer.append(getFrameworkTaskResult().getString());
        if (componentTaskResults != null) {
            Iterator i = componentTaskResults.iterator();
            while (i.hasNext()) {
                ComponentTaskResult componentTaskResult = (ComponentTaskResult) i.next();
                buffer.append(componentTaskResult.getString());
            }
        }
        buffer.append("\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate this instance state.
     *
     * @throws JBIMessageException if the instance state is inconsistent
     */
    public void validate() throws JBIMessageException {
        if (frameworkTaskResult == null) {
            logger.warning("Framework task result has not been set");
        }
    }

    /**
     * Get the framework task result sub-element associated with this instance.
     *
     * @return The framework task result sub-element.
     */
    public FrameworkTaskResult getFrameworkTaskResult() {
        if (frameworkTaskResult == null) {
            frameworkTaskResult = new FrameworkTaskResult();
        }
        return frameworkTaskResult;
    }

    /**
     * Set a previously built framework task result sub-element.
     *
     * @param frameworkTaskResult The new framework task result.
     * @throws JBIMessageException If the new framework task result is null
     */
    public void setFrameworkTaskResult(FrameworkTaskResult frameworkTaskResult) throws JBIMessageException {
        if (frameworkTaskResult == null) {
            throw new JBIMessageException("Framework task result cannot be null");
        }

        this.frameworkTaskResult = frameworkTaskResult;
    }

    /**
     * Get the ordered, possibly empty (but not null) collection of component
     * task result sub-elements associated with this instance.
     *
     * @return The framework task result list.
     */
    public List getComponentTaskResults() {
        if (componentTaskResults == null) {
            componentTaskResults = JBIUtils.createSafeList(ComponentTaskResult.class, false, false);
        }

        return componentTaskResults;
    }

    /**
     * Construct a new, empty <code>ComponentTaskResult</code> instance and
     * add it to the component task result list.
     *
     * @return A new, empty component task result.
     */
    public ComponentTaskResult newComponentTaskResult() {
        ComponentTaskResult componentTaskResult = new ComponentTaskResult();
        getComponentTaskResults().add(componentTaskResult);
        return componentTaskResult;
    }

    /**
     * Add a new component task result to this instance.
     *
     * @param componentTaskResult The new component task result.
     * @throws JBIMessageException If the new component task result is null.
     */
    public void addComponentTaskResult(ComponentTaskResult componentTaskResult) throws JBIMessageException {
        if (componentTaskResult == null) {
            throw new JBIMessageException("Component task result cannot be null");
        }

        getComponentTaskResults().add(componentTaskResult);
    }

    /**
     * Remove a component task result previously added to the list.
     *
     * @param componentTaskResult The component task result to be removed.
     * @throws JBIMessageException If the given component task result is null,
     *                             the component task result list is empty or
     *                             the given component task result was not in
     *                             the list.
     */
    public void removeComponentTaskResult(ComponentTaskResult componentTaskResult) throws JBIMessageException {
        if (componentTaskResult == null) {
            throw new JBIMessageException("Cannot remove null component");
        }

        if (componentTaskResults == null) {
            throw new JBIMessageException("Empty component result list, cannot remove");
        }

        if (!componentTaskResults.remove(componentTaskResult)) {
            throw new JBIMessageException("Component result not in list, cannot remove");
        }
    }

    /**
     * Set a (possibly empty, but not null) list of previously built component
     * task result instances. This list must contain only instances of
     * <code>ComponentTaskResult</code>.
     *
     * @param componentTaskResults The list of component task results
     * @throws JBIMessageException If the component task result list is null or
     *                             any of its element is not actually of type
     *                             <code>ComponentTaskResult</code>.
     */
    public void setComponentTaskResults(List componentTaskResults) throws JBIMessageException {
        JBIUtils.validateCollection(componentTaskResults, ComponentTaskResult.class);
        this.componentTaskResults = componentTaskResults;
    }
}
