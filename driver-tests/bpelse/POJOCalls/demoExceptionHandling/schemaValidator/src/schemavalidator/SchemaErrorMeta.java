/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package schemavalidator;

import org.w3c.dom.Node;

/**
 *
 * @author mpottlapelli
 */
public interface SchemaErrorMeta {
	int errorCount();
	String errorMessage();
        Node getNode();
}
