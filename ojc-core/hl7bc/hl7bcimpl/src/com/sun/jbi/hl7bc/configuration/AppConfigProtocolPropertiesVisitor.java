package com.sun.jbi.hl7bc.configuration;

import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;

public class AppConfigProtocolPropertiesVisitor implements Visitor{
    private final HL7ProtocolProperties mProtocolproperties;

    public AppConfigProtocolPropertiesVisitor(HL7ProtocolProperties protocolproperties) {
        assert protocolproperties != null;
        mProtocolproperties = protocolproperties;
    }

    public void visit(Visitable visitable) {
        synchronized (mProtocolproperties) {
            if (visitable instanceof AppConfigValidateMSHField) {
            	AppConfigValidateMSHField field = (AppConfigValidateMSHField) visitable;
                mProtocolproperties.setValidateMSHEnabled(field.getValue());
            } else if (visitable instanceof AppConfigAckModeField) {
            	AppConfigAckModeField field = (AppConfigAckModeField) visitable;
            	mProtocolproperties.setAckMode(field.getValue());
            }else if (visitable instanceof AppConfigLLPTypeField) {
            	AppConfigLLPTypeField field = (AppConfigLLPTypeField) visitable;
            	mProtocolproperties.setLLPType(field.getValue());
            }else if (visitable instanceof AppConfigSBCharField) {
            	AppConfigSBCharField field = (AppConfigSBCharField) visitable;
            	mProtocolproperties.setStartBlockChar(new Byte(field.getValue().byteValue()));
            }else if (visitable instanceof AppConfigEBCharField) {
            	AppConfigEBCharField field = (AppConfigEBCharField) visitable;
            	mProtocolproperties.setEndBlockChar(new Byte(field.getValue().byteValue()));
            }else if (visitable instanceof AppConfigEDCharField) {
            	AppConfigEDCharField field = (AppConfigEDCharField) visitable;
            	mProtocolproperties.setEndDataChar(new Byte(field.getValue().byteValue()));
            }else if (visitable instanceof AppConfigHLLPChecksumField) {
            	AppConfigHLLPChecksumField field = (AppConfigHLLPChecksumField) visitable;
            	mProtocolproperties.setHLLPChkSumEnabled(field.getValue());
            }else if (visitable instanceof AppConfigMLLPv2RetriesOnNakField) {
            	AppConfigMLLPv2RetriesOnNakField field = (AppConfigMLLPv2RetriesOnNakField) visitable;
            	mProtocolproperties.setMLLPV2RetriesCountOnNak(field.getValue());
            }else if (visitable instanceof AppConfigMLLPV2RetryInterval) {
            	AppConfigMLLPV2RetryInterval field = (AppConfigMLLPV2RetryInterval) visitable;
                mProtocolproperties.setMllpv2RetryInterval(new Long(field.getValue().longValue()));
            }else if (visitable instanceof AppConfigMLLPV2TimeToWaitForAckNak) {
            	AppConfigMLLPV2TimeToWaitForAckNak field = (AppConfigMLLPV2TimeToWaitForAckNak) visitable;
            	mProtocolproperties.setMllpv2TimeToWaitForAckNak(new Long(field.getValue().longValue()));
            }else if (visitable instanceof AppConfigSeqNumEnabledField) {
            	AppConfigSeqNumEnabledField field = (AppConfigSeqNumEnabledField) visitable;
            	mProtocolproperties.setSeqNumEnabled(field.getValue());
            }else if (visitable instanceof AppConfigProcessingIdField) {
            	AppConfigProcessingIdField field = (AppConfigProcessingIdField) visitable;
            	mProtocolproperties.setProcessingID(field.getValue());
            }else if (visitable instanceof AppConfigVersionIdField) {
            	AppConfigVersionIdField field = (AppConfigVersionIdField) visitable;
            	mProtocolproperties.setVersionID(field.getValue());
            }else if (visitable instanceof AppConfigFieldSeparator) {
            	AppConfigFieldSeparator field = (AppConfigFieldSeparator) visitable;
            	mProtocolproperties.setFieldSeparator(Byte.valueOf(field.getValue()));
            }else if (visitable instanceof AppConfigEncodingCharactersField) {
            	AppConfigEncodingCharactersField field = (AppConfigEncodingCharactersField) visitable;
            	mProtocolproperties.setEncodingCharacters(field.getValue());
            }else if (visitable instanceof AppConfigSendingApplicationField) {
            	AppConfigSendingApplicationField field = (AppConfigSendingApplicationField) visitable;
            	mProtocolproperties.setSendingApplication(field.getValue());
            }else if (visitable instanceof AppConfigSendingFacilityField) {
            	AppConfigSendingFacilityField field = (AppConfigSendingFacilityField) visitable;
            	mProtocolproperties.SetSendingFacility(field.getValue());
            }else if (visitable instanceof AppConfigSFTEnabledField) {
            	AppConfigSFTEnabledField field = (AppConfigSFTEnabledField) visitable;
            	mProtocolproperties.setSFTEnabled(field.getValue());
            }else if (visitable instanceof AppConfigSoftVendorOrgField) {
            	AppConfigSoftVendorOrgField field = (AppConfigSoftVendorOrgField) visitable;
            	mProtocolproperties.setSoftwareVendorOrganization(field.getValue());
            }else if (visitable instanceof AppConfigSoftVersionOrReleaseNumField) {
            	AppConfigSoftVersionOrReleaseNumField field = (AppConfigSoftVersionOrReleaseNumField) visitable;
            	mProtocolproperties.setSoftwareCertifiedVersionOrReleaseNumber(field.getValue());
            }else if (visitable instanceof AppConfigSoftProductNameField) {
            	AppConfigSoftProductNameField field = (AppConfigSoftProductNameField) visitable;
            	mProtocolproperties.setSoftwareProductName(field.getValue());
            }else if (visitable instanceof AppConfigSoftBinaryId) {
            	AppConfigSoftBinaryId field = (AppConfigSoftBinaryId) visitable;
            	mProtocolproperties.setSoftwareBinaryID(field.getValue());
            }else if (visitable instanceof AppConfigSoftProdInfoField) {
            	AppConfigSoftProdInfoField field = (AppConfigSoftProdInfoField) visitable;
            	mProtocolproperties.setSoftwareProductInformation(field.getValue());
            }else if (visitable instanceof AppConfigSoftInstallDateField) {
            	AppConfigSoftInstallDateField field = (AppConfigSoftInstallDateField) visitable;
            	mProtocolproperties.setSoftwareInstallDate(field.getValue());
            }else if (visitable instanceof AppConfigJournallingField) {
            	AppConfigJournallingField field = (AppConfigJournallingField) visitable;
            	mProtocolproperties.setJournallingEnabled(field.getValue());
            }
        }
    }
}
