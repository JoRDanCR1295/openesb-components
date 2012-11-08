package com.sun.jbi.httpsoapbc.validator.mimevalidators;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.wsdl.extensions.mime.MIMEMultipartRelated;
import javax.wsdl.extensions.mime.MIMEPart;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPHeader;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.ValidationException;

public class WSIAP10Validator {
	public static final String CONTENT_TYPE_TEXT_XML = "text/xml";
	public static final String ELEM_CONTENT = "content";
	public static final String ELEM_MULTIPART_RELATED = "multipartRelated";
	public static final String ELEM_MIME_XML = "mimeXml";
	private static final Messages mMessages = Messages
			.getMessages(WSIAP10Validator.class);

	public void validate(Binding binding) throws ValidationException {
		List ops = binding.getBindingOperations();
		for (int i = 0; i < ops.size(); i++) {
			BindingOperation bindingOperation = (BindingOperation) ops.get(i);
			BindingInput bindingInput = bindingOperation.getBindingInput();
			BindingOutput bindingOutput = bindingOperation.getBindingOutput();
			testAP2901(bindingInput, bindingOutput);

			List inputMimeContents = getMimeContentElements(bindingInput == null ? new ArrayList()
					: bindingInput.getExtensibilityElements());
			List outputMimeContents = getMimeContentElements(bindingOutput == null ? new ArrayList()
					: bindingOutput.getExtensibilityElements());
			testAP2903_2910_2944(binding, bindingOperation, bindingInput,
					bindingOutput, inputMimeContents, outputMimeContents);

			List inputMultiparts = getMimeMultipartElements(bindingInput == null ? null
					: bindingInput.getExtensibilityElements());
			List outputMultiparts = getMimeMultipartElements(bindingOutput == null ? null
					: bindingOutput.getExtensibilityElements());
			testAP2911_2906(binding, inputMultiparts, outputMultiparts);

			List inputMimeParts = getMimeParts(bindingInput == null ? null
					: bindingInput.getExtensibilityElements());
			List outputMimeParts = getMimeParts(bindingOutput == null ? null
					: bindingOutput.getExtensibilityElements());
			testAP2909(binding, inputMimeParts, outputMimeParts);

			testAP2930(binding, (BindingOperation) ops.get(i));

			// //rule 2940 and rule 2941 are pending
			// /*Input portTypeInput = ((Operation)
			// bindingOperation.getOperation().get()).getInput();
			// Output portTypeOutput = ((Operation)
			// bindingOperation.getOperation().get()).getOutput();
			// testAP2940(portTypeInput, portTypeOutput);
			// testAP2941(binding, bindingOperation, bindingInput,
			// bindingOutput, portTypeInput, portTypeOutput);*/
		}
	}

	/**
	 * Collects all mime:content elements.
	 * 
	 * @param extElems
	 *            a list of extensibility elements that can contain
	 *            mime:contentS.
	 * @return the list of mime:content elements found.
	 */
	private List getMimeContentElements(List extElems) {
		List mimeContentElements = new ArrayList();

		if (extElems != null) {
			// Going through all the extensibility elements
			for (int i = 0; i < extElems.size(); i++) {
				ExtensibilityElement extElem = (ExtensibilityElement) extElems
						.get(i);
				// If the element is mime:multipartRelated
				if (extElem instanceof MIMEMultipartRelated) {
					// Getting the mime:part elements of the
					// mime:multipartRelated
					List mimeParts = ((MIMEMultipartRelated) extElem)
							.getMIMEParts();
					// Going through all the mime:part elements
					for (int j = 0; j < mimeParts.size(); j++) {
						// Collecting all the mime:content elements of this
						// mime:part
						List elems = getMimeContentElements(((MIMEPart) mimeParts
								.get(j)).getExtensibilityElements());
						// Adding the elements to the list being returned
						mimeContentElements.addAll(elems);
					}
				}
				// Else if the element is mime:content
				else if (extElem instanceof MIMEContent) {
					// Adding the element to the list being returned
					mimeContentElements.add(extElem);
				}
			}
		}

		return mimeContentElements;
	}

	/**
	 * Collects all mime:multipartRelated elements.
	 * 
	 * @param extElems
	 *            a list of extensibility elements that can contain
	 *            mime:multipartRelated elements.
	 * @return the list of mime:multipartRelated elements found.
	 */
	private List getMimeMultipartElements(List extElems) {
		List mimeMultipartElements = new ArrayList();

		if (extElems != null) {
			// Going through all the extensibility elements
			for (int i = 0; i < extElems.size(); i++) {
				ExtensibilityElement extElem = (ExtensibilityElement) extElems
						.get(i);
				// If the element is mime:multipartRelated
				if (extElem instanceof MIMEMultipartRelated) {
					// Adding the element to the list being returned
					mimeMultipartElements.add(extElem);
					// Getting the mime:part elements of the
					// mime:multipartRelated
					List mimeParts = ((MIMEMultipartRelated) extElem)
							.getMIMEParts();
					// Going through all the mime:part elements
					for (int j = 0; j < mimeParts.size(); j++) {
						// Collecting all the mime:multipartRelated elements of
						// this mime:part
						List elems = getMimeMultipartElements(((MIMEPart) mimeParts
								.get(j)).getExtensibilityElements());
						// Adding the elements to the list being returned
						mimeMultipartElements.addAll(elems);
					}
				}
			}
		}

		return mimeMultipartElements;
	}

	/**
	 * Collects all mime:part elements.
	 * 
	 * @param extElems
	 *            a list of extensibility elements that can contain mime:part
	 *            elements.
	 * @return the list of mime:part elements found.
	 */
	private List getMimeParts(List extElems) {
		List mimeParts = new ArrayList();

		if (extElems != null) {
			// Going through all the extensibility elements
			for (int i = 0; i < extElems.size(); i++) {
				ExtensibilityElement extElem = (ExtensibilityElement) extElems
						.get(i);
				// If the element is mime:multipartRelated
				if (extElem instanceof MIMEMultipartRelated) {
					// Getting the mime:part elements of the
					// mime:multipartRelated
					List mParts = ((MIMEMultipartRelated) extElem)
							.getMIMEParts();
					mimeParts.addAll(mParts);
					// Going through all the mime:part elements
					for (int j = 0; j < mParts.size(); j++) {
						List elems = getMimeParts(((MIMEPart) mParts.get(j))
								.getExtensibilityElements());
						// Adding the elements to the list being returned
						mimeParts.addAll(elems);
					}
				}
			}
		}
		return mimeParts;
	}

	private void testAP2901(BindingInput bindingInput,
			BindingOutput bindingOutput) {
		// if (bindingInput != null) {
		// int soapNum = 0;
		// if (isSOAPBinding) {
		// soapNum =
		// bindingInput.getExtensibilityElements(SOAPBody.class).size();
		// } else if (isSOAP12Binding) {
		// soapNum =
		// bindingInput.getExtensibilityElements(SOAP12Body.class).size();
		// }
		// int mimeConentNum =
		// bindingInput.getExtensibilityElements(MIMEContent.class).size();
		// int mimeMultipartNum =
		// bindingInput.getExtensibilityElements(MIMEMultipartRelated.class).size();
		// int mimeXMLNum =
		// bindingInput.getExtensibilityElements(MIMEMimeXml.class).size();
		// if (soapNum == 0 && mimeConentNum == 0 && mimeMultipartNum == 0 &&
		// mimeXMLNum == 0) {
		// mResults.add(new Validator.ResultItem(mValidator,
		// Validator.ResultType.ERROR,
		// bindingInput,
		// NbBundle.getMessage(WSIAPValidator.class, "AP2901_INPUT")));
		// }

		// }
		// if (bindingOutput != null) {
		// int soapNum = 0;
		// if (isSOAPBinding) {
		// soapNum =
		// bindingOutput.getExtensibilityElements(SOAPBody.class).size();
		// } else if (isSOAP12Binding) {
		// soapNum =
		// bindingOutput.getExtensibilityElements(SOAP12Body.class).size();
		// }
		// int mimeConentNum =
		// bindingOutput.getExtensibilityElements(MIMEContent.class).size();
		// int mimeMultipartNum =
		// bindingOutput.getExtensibilityElements(MIMEMultipartRelated.class).size();
		// int mimeXMLNum =
		// bindingOutput.getExtensibilityElements(MIMEMimeXml.class).size();
		// if (soapNum == 0 && mimeConentNum == 0 && mimeMultipartNum == 0 &&
		// mimeXMLNum == 0) {
		// mResults.add(new Validator.ResultItem(mValidator,
		// Validator.ResultType.ERROR,
		// bindingOutput,
		// NbBundle.getMessage(WSIAPValidator.class, "AP2901_OUTPUT")));
		// }

		// }
	}

	private void testAP2903_2910_2944(Binding binding,
			BindingOperation bindingOperation, BindingInput bindingInput,
			BindingOutput bindingOutput, List inputMimeContents,
			List outputMimeContents) throws ValidationException {

		if (!inputMimeContents.isEmpty()) {
			Input portTypeInput = bindingOperation.getOperation().getInput();
			if (portTypeInput == null) {
				throw new ValidationException(mMessages
						.getString("HTTPBC-E01191.WSIAP2903_PORTTYPE_NULL"));

			} else if (portTypeInput.getMessage() == null) {
				throw new ValidationException(mMessages
						.getString("HTTPBC-E01191.WSIAP2903_MESSAGE_NULL"));
			} else {

				testInvalidMimeContentPart_2903(binding, inputMimeContents,
						portTypeInput.getMessage());
				testInvalidMimeContentPart_2910(binding, inputMimeContents,
						portTypeInput.getMessage());
				testInvalidMimeContentPart_2944(binding, inputMimeContents,
						portTypeInput.getMessage());

			}
		}

		if (!outputMimeContents.isEmpty()) {
			Output portTypeOutput = bindingOperation.getOperation().getOutput();
			if (portTypeOutput == null) {
				throw new ValidationException(mMessages
						.getString("HTTPBC-E01191.WSIAP2903_PORTTYPE_NULL"));

			} else if (portTypeOutput.getMessage() == null) {
				throw new ValidationException(mMessages
						.getString("HTTPBC-E01191.WSIAP2903_MESSAGE_NULL"));
			} else {
				testInvalidMimeContentPart_2903(binding, outputMimeContents,
						portTypeOutput.getMessage());
				testInvalidMimeContentPart_2910(binding, outputMimeContents,
						portTypeOutput.getMessage());
				testInvalidMimeContentPart_2944(binding, outputMimeContents,
						portTypeOutput.getMessage());
			}
		}
	}

	private void testAP2911_2906(Binding binding, List inputMultiparts,
			List outputMultiparts) throws ValidationException {

		if (!inputMultiparts.isEmpty()) {

			containsBodyHeader_2911_2906(binding, inputMultiparts);
		}

		// If the wsdl:output contains mime:multipartRelated elements
		if (!outputMultiparts.isEmpty()) {
			containsBodyHeader_2911_2906(binding, outputMultiparts);
		}

	}

	private void testAP2909(Binding binding, List inputMimeParts,
			List outputMimeParts) throws ValidationException {
		QName bindingName = binding.getQName();
		List newList = new ArrayList();
		newList.addAll(inputMimeParts);
		newList.addAll(outputMimeParts);
		// Going through a list of mime:part elements
		for (int i = 0; i < newList.size(); i++) {
			// A variable that indicates the mime:part contains
			// at least one mime:content element
			boolean mimeContentFound = false;
			String mimeContentPart = null;

			List extElems = ((MIMEPart) newList.get(i))
					.getExtensibilityElements();
			for (int j = 0; j < extElems.size(); j++) {
				// If an extensibility element is mime:content
				if (((ExtensibilityElement) extElems.get(j)) instanceof MIMEContent) {
					MIMEContent mimeContent = (MIMEContent) extElems.get(j);
					// If a mime:content element was already found in this
					// mime:part
					if (mimeContentFound) {
						// If a mime:content references other wsdl:part than the
						// previous mime:content do, return true
						if (mimeContent.getPart() == null
								|| !mimeContent.getPart().equals(
										mimeContentPart)) {
							throw new ValidationException(mMessages.getString(
									"HTTPBC-E01193.WSIAP2909", bindingName
											.toString()));
						}
					}				
					else {
						mimeContentFound = true;
						mimeContentPart = mimeContent.getPart();
					}
				}
			}
		}
	}

	private void testAP2930(Binding binding, BindingOperation op)
			throws ValidationException {
		QName bindingName = binding.getQName();

		// Getting wsdl:fault elements
		Collection faults = op.getBindingFaults().values();
		if (faults != null) {
			for (Iterator it = faults.iterator(); it.hasNext();) {
				// Getting wsdl:fault's extensibility elements
				List extElems = ((BindingFault) it.next())
						.getExtensibilityElements();
				for (int j = 0; j < extElems.size(); j++) {
					// If there is a mime:multipartRelated element, return true
					if (((ExtensibilityElement) extElems.get(j)) instanceof MIMEMultipartRelated) {
						throw new ValidationException(mMessages.getString(
								"HTTPBC-E01196.WSIAP2930", bindingName
										.toString()));
					}
				}
			}
		}

	}

	private void testInvalidMimeContentPart_2903(Binding binding,
			List mimeContents, Message message) throws ValidationException {
		QName bindingName = binding.getQName();
		// Going throug all the mime:content elements
		for (int i = 0; i < mimeContents.size(); i++) {
			// Getting the part name of a mime:element
			String partName = ((MIMEContent) mimeContents.get(i)).getPart();
			// If the wsdl:message does not contain such part, return the part
			if (!message.getParts().keySet().contains(partName))

				throw new ValidationException(mMessages.getString(
						"HTTPBC-E01191.WSIAP2903", new Object[] {
								bindingName.toString(), partName }));
		}
	}

	private void testInvalidMimeContentPart_2910(Binding binding,
			List mimeContents, Message message) throws ValidationException {
		QName bindingName = binding.getQName();
		// Going throug all the mime:content elements
		for (int i = 0; i < mimeContents.size(); i++) {
			// Getting a value of the part attribute from a mime:element
			String partName = ((MIMEContent) mimeContents.get(i)).getPart();
			// Getting the corresponging wsdl:part
			Part part = message.getPart(partName);
			if (part != null
					&& ((part.getTypeName() == null && part.getElementName() == null) || (part
							.getTypeName() != null && part.getElementName() != null))) {
				throw new ValidationException(mMessages.getString(
						"HTTPBC-E01194.WSIAP2910", new Object[] {
								bindingName.toString(), partName }));
			}
		}
	}

	private void testInvalidMimeContentPart_2944(Binding binding,
			List mimeContents, Message message) throws ValidationException {
		QName bindingName = binding.getQName();
		// Going through a list of mime:content elements
		for (int i = 0; i < mimeContents.size(); i++) {
			MIMEContent mimeContent = (MIMEContent) mimeContents.get(i);
			// Getting the corresponding wsdl:part
			Part part = message.getPart(mimeContent.getPart());
			// If the part is defined with the element attribute
			if (part != null && part.getElementName() != null) {
				// If the type attribute value is other than "text/xml"
				if (!CONTENT_TYPE_TEXT_XML.equals(mimeContent.getType())) {
					// return the invalid element
					throw new ValidationException(mMessages.getString(
							"HTTPBC-E01199.WSIAP2944", new Object[] {
									bindingName.toString(),
									mimeContent.getPart() }));
				}
			}
		}

	}

	private void containsBodyHeader_2911_2906(Binding binding, List multiparts)
			throws ValidationException {
		QName bindingName = binding.getQName();
		// A variable indicates that a soap:body element is found
		boolean soapBodyFound = false;
		// Going through a list of mime:multipartRelated elements
		for (int i = 0; i < multiparts.size(); i++) {
			soapBodyFound = false;
			// Getting a list of mime:part elements
			List mimeParts = ((MIMEMultipartRelated) multiparts.get(i))
					.getMIMEParts();

			// Going through all the mime:part elements
			for (int j = 0; j < mimeParts.size(); j++) {

				// Getting a list of extensibility elements of a mime:part
				List extElems = ((MIMEPart) mimeParts.get(j))
						.getExtensibilityElements();
				boolean hasSOAPBody = false;
				boolean hasSOAPHEADER = false;
				// Going through the extensibility elements
				for (int k = 0; k < extElems.size(); k++) {
					hasSOAPBody = true;
					// If an extensibility element is a soap:body
					if (((ExtensibilityElement) extElems.get(k)) instanceof SOAPBody) {
						// If a soap:body element was already found,
						// return true
						if (soapBodyFound) {
							throw new ValidationException(mMessages.getString(
									"HTTPBC-E01195.WSIAP2911", bindingName
											.toString()));
						}
						// else set the variable to the true value
						else {
							soapBodyFound = true;
						}
					}

					if ((ExtensibilityElement) extElems.get(k) instanceof SOAPHeader) {
						hasSOAPHEADER = true;
					}
				}

				if (!hasSOAPBody && hasSOAPHEADER) {
					throw new ValidationException(mMessages.getString(
							"HTTPBC-E01192.WSIAP2906", bindingName.toString()));
				}

			}

			if (!soapBodyFound) {
				throw new ValidationException(mMessages.getString(
						"HTTPBC-E01195.WSIAP2911", bindingName.toString()));
			}
		}
	}
}
