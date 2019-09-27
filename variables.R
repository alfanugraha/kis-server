# initiate global variable
appsTitle <- 'KIS Satu Peta'
appsMenu <- data.frame(
  menuItem = c('Metadata', 'Contact', 'Spatial Representation Info', 'Reference System Info', 'Identification Info',
               'Distribution Info', 'Transfer Options', 'Metadata Maintenace', 'Metadata Constrains'),
  tabName = c('metadata', 'contact', 'spatialRepresentationInfo', 'referenceSystemInfo', 'identificationInfo',
              'distributionInfo', 'transferOptions', 'metadataMaintenance', 'metadataConstraint')
)
# 1. metadata
mdEntity <- data.frame(
  vars = c('fileIdentifier', 'lang', 'charSet', 'hieLvl', 'mdStdName', 'mdStdVer', 'dateStamp', 'mdDataSetURI'),
  name = c('File Identifier', 'Language', 'Character Set', 'Hierarchy Level', 'Metadata Standard Name', 'Metadata Standard Version', 'Date Stamp', 'Data Set URI'),
  val = c('', '', '', '', '', '', '', ''),
  desc = c('', '', '', '', '', '', '', '')
)
# 2. contact
ctEntity <- data.frame(
  vars = c('indName', 'orgName', 'posName', 'phone', 'fax', 'delivPoint', 'city', 'adminArea', 'postCode', 'country', 'emailAdd', 'linkage', 'protocol', 'contFunction', 'hOfService', 'contInstructions', 'contRole'),
  name = c('Individual Name', 'Organisation Name', 'Position Name', 'Phone', 'Faximile', 'Delivery Point', 'City', 'Administrative Area', 'Postal Code', 'Country', 'Email Address', 'Linkage', 'Protocol', 'Function', 'Hours Of Service', 'Contact Instructions', 'Role'),
  val = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
  desc = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '')
)
# 3. spatial representation info
sriEntity <- data.frame(
  vars = c('topoLvl', 'geomObj', 'nOfDim', 'cornerPoints', 'pointInPxl', 'axisDimProp', 'dimName', 'dimSize', 'cellGeom', 'checkPointAvail', 'controlPointAvail', 'geoParam', 'objType'),
  name = c('Topology Level', 'Geometry Objects', 'Number Of Dimension', 'Corner Points', 'Point in Pixel', 'Axis Dimension Property', 'Dimension Name', 'Dimension Size', 'Cell Geometry', 'Check Point Availability', 'Control Point Availability', 'Georeference Parameters', 'Object Type'),
  val = c('', '', '', '', '', '', '', '', '', '', '', '', ''),
  desc = c('', '', '', '', '', '', '', '', '', '', '', '', '')
)
# 4. reference system info
refSysEntity <- data.frame(
  vars = c('rsTitle', 'rsDate', 'rsDateType', 'rsOrgName', 'rsLinkage', 'rsRole', 'rsCode', 'rsVersion', 'rsName', 'rsCrs', 'rsSemiMajorAxis', 'rsAxisUnit'),
  name = c('Title', 'Date', 'Date Type', 'Organisation Name', 'Linkage', 'Role', 'Code', 'Version', 'Name', 'Coordinate Reference System', 'Semi Major Axis', 'Axis Unit'),
  val = c('', '', '', '', '', '', '', '', '', '', '', ''),
  desc = c('', '', '', '', '', '', '', '', '', '', '', '')
)
# 5. identification info
idInfoEntity <- data.frame(
  vars = c('iiCitation', 'iiTitle', 'iiDate', 'iiDateType', 'iiAbstract', 'iiResMaintenance', 'iiDescKey', 'iiSpatRes', 'iiResCons', 'iiSpatRepType', 'iiLangIdent', 'iiCharSetCode', 'iiTopicCat', 'westBoundLong', 'eastBoundLong', 'southBoundLat', 'northBoundLat'),
  name = c('Citation', 'Title', 'Date', 'Date Type', 'Abstract', 'Resource Maintenance', 'Descriptive Keywords', 'Spatial Resolution', 'Resource Constraints', 'Spatial Representation Type', 'Language Identification', 'Character Set Code', 'Topic Category', 'West Bound Longitude', 'East Bound Longitude', 'South Bound Latitude', 'North Bound Latitude'),
  val = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
  desc = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '')
)
# 6. distribution info
disInfoEntity <- data.frame(
  vars = c('dIndName', 'dOrgName', 'dPosName', 'dPhone', 'dFax', 'dDelivPoint', 'dCity', 'dAdminArea', 'dPostCode', 'dCountry', 'dEmailAdd', 'dLinkage', 'dProtocol', 'dFunction', 'dHOfService', 'dContInstructions', 'dRole'),
  name = c('Individual Name', 'Organisation Name', 'Position Name', 'Phone', 'Faximile', 'Delivery Point', 'City', 'Administrative Area', 'Postal Code', 'Country', 'Email Address', 'Linkage', 'Protocol', 'Function', 'Hours Of Service', 'Contact Instructions', 'Role'),
  val = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
  desc = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '')
)
# 7. transfer options (4): WFS, WMS, ZIP Shapefile, ImageWMS
trfOptWFSEntity <- data.frame(
  vars = c('WFSLinkage', 'WFSProtocol', 'WFSName', 'WFSDesc', 'WFSFunc'),
  name = c('Linkage', 'Protocol', 'Name', 'Description', 'Function'),
  val = c('', '', '', '', ''),
  desc = c('', '', '', '', '')
)
trfOptWMSEntity <- data.frame(
  vars = c('WMSLinkage', 'WMSProtocol', 'WMSName', 'WMSDesc', 'WMSFunc'),
  name = c('Linkage', 'Protocol', 'Name', 'Description', 'Function'),
  val = c('', '', '', '', ''),
  desc = c('', '', '', '', '')
) 
trfOptZIPEntity <- data.frame(
  vars = c('ZIPLinkage', 'ZIPProtocol', 'ZIPName', 'ZIPDesc', 'ZIPFunc'),
  name = c('Linkage', 'Protocol', 'Name', 'Description', 'Function'),
  val = c('', '', '', '', ''),
  desc = c('', '', '', '', '')
)
trfOptIWMSEntity <- data.frame(
  vars = c('IWMSLinkage', 'IWMSProtocol', 'IWMSName', 'IWMSDesc', 'IWMSFunc'),
  name = c('Linkage', 'Protocol', 'Name', 'Description', 'Function'),
  val = c('', '', '', '', ''),
  desc = c('', '', '', '', '')
)
# 8. metadata maintenance
mdMtncEntity <- data.frame(
  vars = c('updFreq', 'note'),
  name = c('Maintenance and Update Frequency', 'Maintenance Note'),
  val = c('', ''),
  desc = c('', '')
)
# 9. metadata constraints
mdConstEntity <- data.frame(
  vars = c('class', 'userNote', 'userLimit'),
  name = c('Classification', 'User Note', 'User Limitation'),
  val = c('', '', ''),
  desc = c('', '', '')
)