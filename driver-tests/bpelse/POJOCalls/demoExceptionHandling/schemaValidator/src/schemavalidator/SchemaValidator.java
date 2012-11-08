/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package schemavalidator;

import java.io.File;
import javax.xml.XMLConstants;
import javax.xml.validation.Validator;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.SchemaFactory;
import org.w3c.dom.Node;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXParseException;

/**
 *
 * @author mpottlapelli
 */
public class SchemaValidator {

    /**
     * @param args the command line arguments
     */
    public static boolean validate(String schema, Node doc) throws ErrorCounter {
        SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        ErrorCounter errorCounter = new ErrorCounter(schema, doc);

        try {
            javax.xml.validation.Schema valSchema = null;
            valSchema = sf.newSchema(new File(schema).toURI().toURL());
            Validator val = valSchema.newValidator();
            val.setErrorHandler(errorCounter);
            val.validate(new DOMSource(doc));
            if (errorCounter.errorCount > 0) {
                throw errorCounter;
            }
            return true;
        } catch (Exception ex) {
            if (ex instanceof ErrorCounter) {
                throw (ErrorCounter) ex;
            }
            throw new ErrorCounter(ex);
        }
    }

    public static class ErrorCounter extends Exception implements ErrorHandler, SchemaErrorMeta {

        public ErrorCounter(String schema, Node doc) {
            super();
            errorMsg = "Validation errors as per schema: \"" + schema + "\"";
            this.doc = doc;
        }

        public ErrorCounter(Exception ex) {
            super(ex);
        }
        int errorCount = 0;
        String errorMsg;
        Node doc;

        public void error(SAXParseException e) {
            errorMsg = errorMsg + e.getMessage();
            errorCount++;
        }

        public void fatalError(SAXParseException e) {
            errorMsg = errorMsg + e.getMessage();
            errorCount++;
        }

        public void warning(SAXParseException e) {
            // do nothing
        }

        @Override
        public String getMessage() {
            return errorMsg;
        }

        @Override
        public String toString() {
            return errorMsg;
        }

        public int errorCount() {
            return errorCount;
        }

        public String errorMessage() {
            return errorMsg;
        }

        public Node getNode() {
            return doc;
        }
    }
}

