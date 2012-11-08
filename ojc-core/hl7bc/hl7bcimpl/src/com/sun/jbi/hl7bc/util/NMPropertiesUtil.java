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
 */

/*
 * @(#)NMPropertiesUtil.java
 *
 * Copyright 2007-2010 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.util;

import java.util.Map;
import java.util.HashMap;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.NormalizedMessage;
import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;

public class NMPropertiesUtil {

    public static int MIN_PORT_NUMBER = 0;

    public static int MAX_PORT_NUMBER = 65535;

    public static byte MIN_BLOCK_CHARACTER = 1;

    public static byte MAX_BLOCK_CHARACTER = 127;

    //Common NM Properties
    public static final String NM_PROP_MESSAGE_ID = "org.glassfish.openesb.messaging.messageid";

    public static final String NM_PROP_EP_NAME = "org.glassfish.openesb.exchange.endpointname";

    // HL7 BC specific NM Properties:
    public static final String NM_PROP_HL7BC_USE_DYN_EP_BINDING = "org.glassfish.openesb.hl7.use.dynamic.endpoint";

    //Inbound NM Properties
    //Client Properties
    public static final String CLIENT_ADDRESS = "org.glassfish.openesb.hl7.inbound.client.address";

    //hl7:address
    public static final String INBOUND_HOST = "org.glassfish.openesb.hl7.inbound.address.host";

    public static final String INBOUND_PORT = "org.glassfish.openesb.hl7.inbound.address.port";

    //hl7:protocolproperties
    public static final String INBOUND_SEQNUM_ENABLED = "org.glassfish.openesb.hl7.inbound.protocolproperties.seqnumenabled";

    // Outbound NM Properties
    // hl7:address
    public static final String OUTBOUND_HOST = "org.glassfish.openesb.hl7.outbound.address.host";

    public static final String OUTBOUND_PORT = "org.glassfish.openesb.hl7.outbound.address.port";

    // hl7:protcolproperties
    public static final String OUTBOUND_LLPTYPE = "org.glassfish.openesb.hl7.outbound.protocolproperties.llptype";

    public static final String OUTBOUND_START_BLOCK_CHAR = "org.glassfish.openesb.hl7.outbound.protocolproperties.startblockcharacter";

    public static final String OUTBOUND_END_DATA_CHAR = "org.glassfish.openesb.hl7.outbound.protocolproperties.enddatacharacter";

    public static final String OUTBOUND_END_BLOCK_CHAR = "org.glassfish.openesb.hl7.outbound.protocolproperties.endblockcharacter";

    public static final String OUTBOUND_HLLP_CHECKSUM_ENABLED = "org.glassfish.openesb.hl7.outbound.protocolproperties.hllpchecksumenabled";

    public static final String OUTBOUND_SEQNUM_ENABLED = "org.glassfish.openesb.hl7.outbound.protocolproperties.seqnumenabled";

    public static final String OUTBOUND_PROCESSINGID = "org.glassfish.openesb.hl7.outbound.protocolproperties.processingid";

    public static final String OUTBOUND_VALIDATEMSH = "org.glassfish.openesb.hl7.outbound.protocolproperties.validatemsh";

    public static final String OUTBOUND_ENABLESFT = "org.glassfish.openesb.hl7.outbound.protocolproperties.enablesft";

    public static final String OUTBOUND_SOFTWARE_VENDOR_ORG = "org.glassfish.openesb.hl7.outbound.protocolproperties.softwarevendororganization";

    public static final String OUTBOUND_SOFTWARE_CERTIFIED_VERSIONORRELEASENUMBER = "org.glassfish.openesb.hl7.outbound.protocolproperties.softwarecertifiedversionorreleasenumber";

    public static final String OUTBOUND_SOFTWARE_PRODUCTNAME = "org.glassfish.openesb.hl7.outbound.protocolproperties.softwareproductname";

    public static final String OUTBOUND_SOFTWARE_BINARYID = "org.glassfish.openesb.hl7.outbound.protocolproperties.softwarebinaryid";

    public static final String OUTBOUND_SOFTWARE_PRODUCTINFORMATION = "org.glassfish.openesb.hl7.outbound.protocolproperties.softwareproductinformation";

    public static final String OUTBOUND_SOFTWARE_INSTALLDATE = "org.glassfish.openesb.hl7.outbound.protocolproperties.softwareinstalldate";

    public static final String OUTBOUND_MLLPV2_RETRIESCOUNTONNAK = "org.glassfish.openesb.hl7.outbound.protocolproperties.mllpv2retriescountonnak";

    public static final String OUTBOUND_MLLPV2_RETRYINTERVAL = "org.glassfish.openesb.hl7.outbound.protocolproperties.mllpv2retryinterval";

    public static final String OUTBOUND_MLLPV2_TIMETOWAITFORACKNAK = "org.glassfish.openesb.hl7.outbound.protocolproperties.mllpv2timetowaitforacknak";

    private static final Logger mLog = Logger.getLogger(NMPropertiesUtil.class.getName());
    
    public static Map extractNMProperties(HL7Address address, HL7ProtocolProperties protocolProps) {
        Map<String, String> nmProps = new HashMap<String, String>();
        nmProps.put(INBOUND_HOST, address.getHL7ServerLocation().toString());
        nmProps.put(INBOUND_PORT, address.getHL7ServerPort().toString());
        nmProps.put(INBOUND_SEQNUM_ENABLED, protocolProps.getSeqNumEnabled().toString());
        return nmProps;
    }

    public static HL7Address fabricateAddress(NMProperties nmProps, HL7Address oldAddress)
            throws NMPropertiesParsingException {
        NMPropertiesImpl nmProperties = (NMPropertiesImpl) nmProps;
        HL7Address newAddress = null;
        if (nmProperties.isNewAddressPropertyAvailable()) {
            newAddress = new HL7Address();
            newAddress.setHL7ServerLocation(nmProperties.getHost() != null ? nmProperties.getHost()
                    : oldAddress.getHL7ServerLocation());
            newAddress.setHL7ServerPort(nmProperties.getPort() != -1 ? nmProperties.getPort()
                    : oldAddress.getHL7ServerPort());
            newAddress.setTransportProtocolName(oldAddress.getTransportProtocolName());
            if (mLog.isLoggable(Level.FINE)) {
                // dump the fabricated object
                mLog.log(Level.FINE, I18n.msg("fabricateAddress() - hl7:address [ {0} ] - created instance : {1}"
							 + "\n\nhl7:address [ {2} ] - original instance : {3} ", newAddress, newAddress.toString(), oldAddress,
							 oldAddress.toString()));
            }
        } else {
        	newAddress = oldAddress;
        }
        return newAddress;
    }

    public static HL7ProtocolProperties fabricateProtocolProperties(NMProperties nmProps,
                                                                    HL7ProtocolProperties oldProtocolProps)
            throws NMPropertiesParsingException {
        NMPropertiesImpl nmProperties = (NMPropertiesImpl) nmProps;
        HL7ProtocolProperties newProtocolProps = null;
        if (nmProperties.isNewProtocolPropertyAvailable()) {
            newProtocolProps = new HL7ProtocolProperties();
            newProtocolProps.setAckMode(oldProtocolProps.getAckMode());
            newProtocolProps.setLLPType(nmProperties.getLLPType() != null ? nmProperties.getLLPType()
                    : oldProtocolProps.getLLPType());
            newProtocolProps.setStartBlockChar(nmProperties.getStartBlockChar() != -1 ? nmProperties.getStartBlockChar()
                    : oldProtocolProps.getStartBlockChar());
            newProtocolProps.setEndBlockChar(nmProperties.getEndBlockChar() != -1 ? nmProperties.getEndBlockChar()
                    : oldProtocolProps.getEndBlockChar());
            newProtocolProps.setEndDataChar(nmProperties.getEndDataChar() != -1 ? nmProperties.getEndDataChar()
                    : oldProtocolProps.getEndDataChar());
            newProtocolProps.setHLLPChkSumEnabled(nmProperties.getHLLPChkSumEnabled() ? nmProperties.getHLLPChkSumEnabled()
                    : oldProtocolProps.getHLLPChkSumEnabled());
            newProtocolProps.setSeqNumEnabled(nmProperties.getSeqNumEnabled() ? nmProperties.getSeqNumEnabled()
                    : oldProtocolProps.getSeqNumEnabled());
            newProtocolProps.setValidateMSHEnabled(nmProperties.getValidateMSHEnabled() ? nmProperties.getValidateMSHEnabled()
                    : oldProtocolProps.getValidateMSHEnabled());
            newProtocolProps.setFieldSeparator(oldProtocolProps.getFieldSeparator());
            newProtocolProps.setEncodingCharacters(oldProtocolProps.getEncodingCharacters());
            newProtocolProps.setProcessingID(nmProperties.getProcessingID() != null ? nmProperties.getProcessingID()
                    : oldProtocolProps.getProcessingID());
            newProtocolProps.setVersionID(oldProtocolProps.getVersionID());
            newProtocolProps.setSFTEnabled(nmProperties.getSFTEnabled() ? nmProperties.getSFTEnabled()
                    : oldProtocolProps.getSFTEnabled());

            newProtocolProps.setSoftwareVendorOrganization(nmProperties.getSFTEnabled()
                    && nmProperties.getSoftwareVendorOrganization() != null ? nmProperties.getSoftwareVendorOrganization()
                    : oldProtocolProps.getSoftwareVendorOrganization());
            newProtocolProps.setSoftwareCertifiedVersionOrReleaseNumber(nmProperties.getSFTEnabled()
                    && nmProperties.getSoftwareCertifiedVersionOrReleaseNumber() != null ? nmProperties.getSoftwareCertifiedVersionOrReleaseNumber()
                    : oldProtocolProps.getSoftwareCertifiedVersionOrReleaseNumber());
            newProtocolProps.setSoftwareProductName(nmProperties.getSFTEnabled()
                    && nmProperties.getSoftwareProductName() != null ? nmProperties.getSoftwareProductName()
                    : oldProtocolProps.getSoftwareProductName());
            newProtocolProps.setSoftwareBinaryID(nmProperties.getSFTEnabled()
                    && nmProperties.getSoftwareBinaryID() != null ? nmProperties.getSoftwareBinaryID()
                    : oldProtocolProps.getSoftwareBinaryID());
            newProtocolProps.setSoftwareProductInformation(nmProperties.getSFTEnabled()
                    && nmProperties.getSoftwareProductInformation() != null ? nmProperties.getSoftwareProductInformation()
                    : oldProtocolProps.getSoftwareProductInformation());
            newProtocolProps.setSoftwareInstallDate(nmProperties.getSFTEnabled()
                    && nmProperties.getSoftwareInstallDate() != null ? nmProperties.getSoftwareInstallDate()
                    : oldProtocolProps.getSoftwareInstallDate());

            newProtocolProps.setSendingApplication(oldProtocolProps.getSendingApplication());
            newProtocolProps.SetSendingFacility(oldProtocolProps.getSendingFacility());
            if (nmProperties.getLLPType() != null && nmProperties.getLLPType().equals(HL7Constants.MLLPv2)) {
                newProtocolProps.setMLLPV2RetriesCountOnNak(nmProperties.getMLLPV2RetriesCountOnNak() != -1 ? nmProperties.getMLLPV2RetriesCountOnNak()
                        : oldProtocolProps.getMLLPV2RetriesCountOnNak());
                newProtocolProps.setMllpv2RetryInterval(nmProperties.getMllpv2RetryInterval() != -1 ? nmProperties.getMllpv2RetryInterval()
                        : oldProtocolProps.getMllpv2RetryInterval());
                newProtocolProps.setMllpv2TimeToWaitForAckNak(nmProperties.getMllpv2TimeToWaitForAckNak() != -1 ? nmProperties.getMllpv2TimeToWaitForAckNak()
                        : oldProtocolProps.getMllpv2TimeToWaitForAckNak());
            }

            if (mLog.isLoggable(Level.FINE)) {
                // dump the fabricated object
                mLog.log(Level.FINE, I18n.msg("fabricateProtocolProperties() - hl7:protocolproperties [ {0} "
                        + "] - created instance : {1} \n\nhl7:protocolproperties [ {2} "
                        + "] - original instance : {3} ", newProtocolProps, newProtocolProps.toString(), oldProtocolProps,
							oldProtocolProps.toString()));
            }
        } else {
        	newProtocolProps = oldProtocolProps;
        }
        return newProtocolProps;
    }

    public static OutboundNMProperties getOutBoundNMProperties(NormalizedMessage ex)
            throws NMPropertiesParsingException {
        return new OutBoundNMPropertiesImpl(ex);
    }

    private static String ifNotNullTrim(Object str) {
        String value = (String) str;
        if (value != null && !(value = value.trim()).equals("")) {
            return value;
        }

        return null;
    }

    // marker interface
    public interface NMProperties {
    }

    public interface OutboundNMProperties extends NMProperties {
        public String getHost();

        public int getPort();

        public String getLLPType();

        public byte getStartBlockChar();

        public byte getEndBlockChar();

        public byte getEndDataChar();

        public boolean getHLLPChkSumEnabled();

        public boolean getSeqNumEnabled();

        public String getProcessingID();

        public boolean getValidateMSHEnabled();

        public boolean getSFTEnabled();

        public String getSoftwareVendorOrganization();

        public String getSoftwareCertifiedVersionOrReleaseNumber();

        public String getSoftwareProductName();

        public String getSoftwareBinaryID();

        public String getSoftwareProductInformation();

        public String getSoftwareInstallDate();

        public int getMLLPV2RetriesCountOnNak();

        public long getMllpv2RetryInterval();

        public long getMllpv2TimeToWaitForAckNak();

    }

    private static class OutBoundNMPropertiesImpl extends NMPropertiesImpl implements OutboundNMProperties {
        private OutBoundNMPropertiesImpl(NormalizedMessage ex) throws NMPropertiesParsingException {
            setHost(ex);
            setPort(ex);
            setLLPType(ex);
            setStartBlockChar(ex);
            setEndBlockChar(ex);
            setEndDataChar(ex);
            setHLLPChkSumEnabled(ex);
            setSeqNumEnabled(ex);
            setProcessingID(ex);
            setValidateMSHEnabled(ex);
            setSFTEnabled(ex);
            setSoftwareVendorOrganization(ex);
            setSoftwareCertifiedVersionOrReleaseNumber(ex);
            setSoftwareProductName(ex);
            setSoftwareBinaryID(ex);
            setSoftwareProductInformation(ex);
            setSoftwareInstallDate(ex);
            setMLLPV2RetriesCountOnNak(ex);
            setMllpv2RetryInterval(ex);
            setMllpv2TimeToWaitForAckNak(ex);
            // validate();
        }
    }

    private static class NMPropertiesImpl {
        private String host;

        private int port = -1;

        private String llpType;

        private byte startBlockChar = -1;

        private byte endBlockChar = -1;

        private byte endDataChar = -1;

        private boolean hllpChecksumEnabled;

        private boolean seqNumberEnabled;

        private String processingID;

        private boolean validateMSHEnabled;

        private boolean sftEnabled;

        private String softwareVendorOrganization;

        private String softwareCertifiedVersionOrReleaseNumber;

        private String softwareProductName;

        private String softwareBinaryID;

        private String softwareProductInformation;

        private String softwareInstallDate;

        private int mllpv2RetriesCountonNak = -1;

        private long mllpv2RetryInterval = -1;

        private long mllpv2TimeToWaitForAckNak = -1;

        private boolean newHL7AddressPropertiesAvailable = false;

        private boolean newHL7ProtocolPropertiesAvailable = false;

        private NMPropertiesImpl() {
        }

        public String getHost() {
            return host;
        }

        public void setHost(NormalizedMessage ex) {
            this.host = ifNotNullTrim(ex.getProperty(OUTBOUND_HOST));
            turnOnNewHL7AddressPropertiesAvailable(host);
        }

        public int getPort() {
            return port;
        }

        public void setPort(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_PORT)) != null) {
                this.port = getIntNumberValue(ex, OUTBOUND_PORT);
                if (port < MIN_PORT_NUMBER || port > MAX_PORT_NUMBER) {
                    throw new NMPropertiesParsingException(I18n.msg(
                            "E0323: A value of {0} is not valid for property {1}. The valid range is {2} - {3}.", port, OUTBOUND_PORT,
                                    new Integer(MIN_PORT_NUMBER), new Integer(MAX_PORT_NUMBER) ));
                }
                turnOnNewHL7AddressPropertiesAvailable(Integer.toString(port));
            }
        }

        public String getLLPType() {
            return llpType;
        }

        public void setLLPType(NormalizedMessage ex) throws NMPropertiesParsingException {
            this.llpType = ifNotNullTrim(ex.getProperty(OUTBOUND_LLPTYPE));
            if (llpType != null) {
                isValidLLPType(OUTBOUND_LLPTYPE, llpType);
                turnOnNewHL7ProtocolPropertiesAvailable(llpType);
            }
        }

        public byte getStartBlockChar() {
            return startBlockChar;
        }

        public void setStartBlockChar(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_START_BLOCK_CHAR)) != null) {
                this.startBlockChar = getByteNumberValue(ex, OUTBOUND_START_BLOCK_CHAR);
                if (startBlockChar < MIN_BLOCK_CHARACTER || startBlockChar > MAX_BLOCK_CHARACTER) {
                    throw new NMPropertiesParsingException(I18n.msg(
                            "E0323: A value of {0} is not valid for property {1}. The valid range is {2} - {3}.", startBlockChar,
                                    OUTBOUND_START_BLOCK_CHAR, new Byte(MIN_BLOCK_CHARACTER),
                                    new Byte(MAX_BLOCK_CHARACTER) ));
                }
                turnOnNewHL7ProtocolPropertiesAvailable(Byte.toString(startBlockChar));
            }
        }

        public byte getEndBlockChar() {
            return endBlockChar;
        }

        public void setEndBlockChar(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_END_BLOCK_CHAR)) != null) {
                this.endBlockChar = getByteNumberValue(ex, OUTBOUND_END_BLOCK_CHAR);
                if (endBlockChar < MIN_BLOCK_CHARACTER || endBlockChar > MAX_BLOCK_CHARACTER) {
                    throw new NMPropertiesParsingException(I18n.msg(
                            "E0323: A value of {0} is not valid for property {1}. The valid range is {2} - {3}.", endBlockChar,
                                    OUTBOUND_END_BLOCK_CHAR, new Byte(MIN_BLOCK_CHARACTER),
                                    new Byte(MAX_BLOCK_CHARACTER) ));
                }
                turnOnNewHL7ProtocolPropertiesAvailable(Byte.toString(endBlockChar));
            }
        }

        public byte getEndDataChar() {
            return endDataChar;
        }

        public void setEndDataChar(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_END_DATA_CHAR)) != null) {
                this.endDataChar = getByteNumberValue(ex, OUTBOUND_END_DATA_CHAR);
                if (endDataChar < MIN_BLOCK_CHARACTER || endDataChar > MAX_BLOCK_CHARACTER) {
                    throw new NMPropertiesParsingException(I18n.msg(
                            "E0323: A value of {0} is not valid for property {1}. The valid range is {2} - {3}.",  endDataChar,
                                    OUTBOUND_END_DATA_CHAR, new Byte(MIN_BLOCK_CHARACTER),
                                    new Byte(MAX_BLOCK_CHARACTER) ));
                }
                turnOnNewHL7ProtocolPropertiesAvailable(Byte.toString(endDataChar));
            }
        }

        public boolean getHLLPChkSumEnabled() {
            return hllpChecksumEnabled;
        }

        public void setHLLPChkSumEnabled(NormalizedMessage ex) {
            String str = ifNotNullTrim(ex.getProperty(OUTBOUND_HLLP_CHECKSUM_ENABLED));
            if (str != null) {
                this.hllpChecksumEnabled = Boolean.parseBoolean(str);
            }
            turnOnNewHL7ProtocolPropertiesAvailable(str);
        }

        public boolean getSeqNumEnabled() {
            return seqNumberEnabled;
        }

        public void setSeqNumEnabled(NormalizedMessage ex) {
            String str = ifNotNullTrim(ex.getProperty(OUTBOUND_SEQNUM_ENABLED));
            if (str != null) {
                this.seqNumberEnabled = Boolean.parseBoolean(str);
            }
            turnOnNewHL7ProtocolPropertiesAvailable(str);
        }

        public String getProcessingID() {
            return processingID;
        }

        public void setProcessingID(NormalizedMessage ex) throws NMPropertiesParsingException {
            this.processingID = ifNotNullTrim(ex.getProperty(OUTBOUND_PROCESSINGID));
            if (processingID != null) {
                isValidProcessingID(OUTBOUND_PROCESSINGID, processingID);
            }
            turnOnNewHL7ProtocolPropertiesAvailable(processingID);
        }

        public boolean getValidateMSHEnabled() {
            return validateMSHEnabled;
        }

        public void setValidateMSHEnabled(NormalizedMessage ex) {
            String str = ifNotNullTrim(ex.getProperty(OUTBOUND_VALIDATEMSH));
            if (str != null) {
                this.validateMSHEnabled = Boolean.parseBoolean(str);
            }
            turnOnNewHL7ProtocolPropertiesAvailable(str);
        }

        public boolean getSFTEnabled() {
            return sftEnabled;
        }

        public void setSFTEnabled(NormalizedMessage ex) {
            String str = ifNotNullTrim(ex.getProperty(OUTBOUND_ENABLESFT));
            if (str != null) {
                this.sftEnabled = Boolean.parseBoolean(str);
            }
            turnOnNewHL7ProtocolPropertiesAvailable(str);
        }

        public String getSoftwareVendorOrganization() {
            return softwareVendorOrganization;
        }

        public void setSoftwareVendorOrganization(NormalizedMessage ex) {
            this.softwareVendorOrganization = ifNotNullTrim(ex.getProperty(OUTBOUND_SOFTWARE_VENDOR_ORG));
            turnOnNewHL7ProtocolPropertiesAvailable(softwareVendorOrganization);
        }

        public String getSoftwareCertifiedVersionOrReleaseNumber() {
            return softwareCertifiedVersionOrReleaseNumber;
        }

        public void setSoftwareCertifiedVersionOrReleaseNumber(NormalizedMessage ex) {
            this.softwareCertifiedVersionOrReleaseNumber = ifNotNullTrim(ex.getProperty(OUTBOUND_SOFTWARE_CERTIFIED_VERSIONORRELEASENUMBER));
            turnOnNewHL7ProtocolPropertiesAvailable(softwareCertifiedVersionOrReleaseNumber);
        }

        public String getSoftwareProductName() {
            return softwareProductName;
        }

        public void setSoftwareProductName(NormalizedMessage ex) {
            this.softwareProductName = ifNotNullTrim(ex.getProperty(OUTBOUND_SOFTWARE_PRODUCTNAME));
            turnOnNewHL7ProtocolPropertiesAvailable(softwareProductName);
        }

        public String getSoftwareBinaryID() {
            return softwareBinaryID;
        }

        public void setSoftwareBinaryID(NormalizedMessage ex) {
            this.softwareBinaryID = ifNotNullTrim(ex.getProperty(OUTBOUND_SOFTWARE_BINARYID));
            turnOnNewHL7ProtocolPropertiesAvailable(softwareBinaryID);
        }

        public String getSoftwareProductInformation() {
            return softwareProductInformation;
        }

        public void setSoftwareProductInformation(NormalizedMessage ex) {
            this.softwareProductInformation = ifNotNullTrim(ex.getProperty(OUTBOUND_SOFTWARE_PRODUCTINFORMATION));
            turnOnNewHL7ProtocolPropertiesAvailable(softwareProductInformation);
        }

        public String getSoftwareInstallDate() {
            return softwareInstallDate;
        }

        public void setSoftwareInstallDate(NormalizedMessage ex) {
            this.softwareInstallDate = ifNotNullTrim(ex.getProperty(OUTBOUND_SOFTWARE_INSTALLDATE));
            turnOnNewHL7ProtocolPropertiesAvailable(softwareInstallDate);
        }

        public int getMLLPV2RetriesCountOnNak() {
            return mllpv2RetriesCountonNak;
        }

        public void setMLLPV2RetriesCountOnNak(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_MLLPV2_RETRIESCOUNTONNAK)) != null) {
                this.mllpv2RetriesCountonNak = getIntNumberValue(ex, OUTBOUND_MLLPV2_RETRIESCOUNTONNAK);
                turnOnNewHL7ProtocolPropertiesAvailable(Integer.toString(mllpv2RetriesCountonNak));
            }
        }

        public long getMllpv2RetryInterval() {
            return mllpv2RetryInterval;
        }

        public void setMllpv2RetryInterval(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_MLLPV2_RETRYINTERVAL)) != null) {
                this.mllpv2RetryInterval = getLongNumberValue(ex, OUTBOUND_MLLPV2_RETRYINTERVAL);
                turnOnNewHL7ProtocolPropertiesAvailable(Long.toString(mllpv2RetryInterval));
            }
        }

        public long getMllpv2TimeToWaitForAckNak() {
            return mllpv2TimeToWaitForAckNak;
        }

        public void setMllpv2TimeToWaitForAckNak(NormalizedMessage ex) throws NMPropertiesParsingException {
            if (ifNotNullTrim(ex.getProperty(OUTBOUND_MLLPV2_TIMETOWAITFORACKNAK)) != null) {
                this.mllpv2TimeToWaitForAckNak = getLongNumberValue(ex, OUTBOUND_MLLPV2_TIMETOWAITFORACKNAK);
                turnOnNewHL7ProtocolPropertiesAvailable(Long.toString(mllpv2TimeToWaitForAckNak));
            }
        }

        public long getLongNumberValue(NormalizedMessage ex, String property) throws NMPropertiesParsingException {
            String value = ifNotNullTrim(ex.getProperty(property));
            long result = -1;
            if (value == null)
                return result;
            try {
                result = Long.parseLong(value);
            } catch (Throwable t) {
                String msg = I18n.msg("E0324: Invalid normalized property {0} with value {1}", property,
                        value );
                throw new NMPropertiesParsingException(msg, t);
            }
            return result;
        }

        public int getIntNumberValue(NormalizedMessage ex, String property) throws NMPropertiesParsingException {
            String value = ifNotNullTrim(ex.getProperty(property));
            int result = -1;
            if (value == null)
                return result;
            try {
                result = Integer.parseInt(value);
            } catch (Throwable t) {
                String msg = I18n.msg("E0324: Invalid normalized property {0} with value {1}",  property,
                        value );
                throw new NMPropertiesParsingException(msg, t);
            }
            return result;
        }

        public byte getByteNumberValue(NormalizedMessage ex, String property) throws NMPropertiesParsingException {
            String value = ifNotNullTrim(ex.getProperty(property));
            byte result = -1;
            if (value == null)
                return result;
            try {
                result = Byte.parseByte(value);
            } catch (Throwable t) {
                String msg = I18n.msg("E0324: Invalid normalized property {0} with value {1}",  property,
                        value);
                throw new NMPropertiesParsingException(msg, t);
            }
            return result;
        }

        public boolean isNewProtocolPropertyAvailable() {
            return newHL7ProtocolPropertiesAvailable;
        }

        public boolean isNewAddressPropertyAvailable() {
            return newHL7AddressPropertiesAvailable;
        }

        private void isValidProcessingID(String property, String procID) throws NMPropertiesParsingException {
            if (procID == null)
                return;

            if (!procID.equalsIgnoreCase("P") && !procID.equalsIgnoreCase("D") && !procID.equalsIgnoreCase("T"))
                throw new NMPropertiesParsingException(I18n.msg("E0324: Invalid normalized property {0} with value {1}",
                        property, procID ));
        }

        private void isValidLLPType(String property, String llpType) throws NMPropertiesParsingException {
            if (llpType == null)
                return;

            if (!llpType.equals(HL7Constants.MLLPv1) && !llpType.equals(HL7Constants.MLLPv2)
                    && !llpType.equals(HL7Constants.HLLP))
                throw new NMPropertiesParsingException(I18n.msg("E0324: Invalid normalized property {0} with value {1}",
                         property, llpType ));
        }

        private void turnOnNewHL7AddressPropertiesAvailable(String str) {
            if (str != null)
                newHL7AddressPropertiesAvailable = true;
        }

        private void turnOnNewHL7ProtocolPropertiesAvailable(String str) {
            if (str != null)
                newHL7ProtocolPropertiesAvailable = true;
        }

    }

    public static class NMPropertiesParsingException extends Exception {
        public NMPropertiesParsingException() {
            super();
        }

        public NMPropertiesParsingException(String message) {
            super(message);
        }

        public NMPropertiesParsingException(String message, Throwable cause) {
            super(message, cause);
        }

        public NMPropertiesParsingException(Throwable cause) {
            super(cause);
        }
    }

}
