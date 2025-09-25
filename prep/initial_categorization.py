from googleapiclient.errors import HttpError
import os
import pdb

import pandas as pd

from global_helpers.helpers import build_service, load_config

settings = load_config()

DOWNLOAD_SHEET = True
UPDATE_PROPOSED_SHEET = True
HELPERS_SUBDIR = os.path.join(settings['HELPERS_PATH'], 'delivery_location_remapping')
DOWNLOAD_PATH = os.path.join(HELPERS_SUBDIR, 'map_maternal.csv')
MAP_PATH = os.path.join(HELPERS_SUBDIR, 'delivery_locations_map.csv')


def get_proposed_level(row):
    if row['flag_level_split']:
        return 'split'
    elif row['flag_level_hosp']:
        return 'hosp'
    elif row['flag_level_prim']:
        return 'prim'
    elif row['flag_level_home']:
        return 'home'
    else:
        return 'none'
    
    
def get_proposed_ownership(row):
    if row['flag_ownership_ngo']:
        return 'ngo'
    elif row['flag_ownership_public']:
        return 'pub'
    elif row['flag_ownership_private']:
        return 'priv'
    else:
        return 'no'


def get_proposed_cig(row):
    '''
    There is some custom logic to determin the propsed cig to return.
    If level is "home" or "other", then no need to include ownership if ownership is "no".
    '''
    if row['proposed_level'] == 'home' or row['proposed_level'] == 'other':
        return f'{row["proposed_level"]}|delivery_location'
    else:
        return f'{row["proposed_ownership"]}_{row["proposed_level"]}|delivery_location'


if __name__ == '__main__':
    if not os.path.exists(HELPERS_SUBDIR):
        os.makedirs(HELPERS_SUBDIR)
    # Read in the existing location mapping (from map_maternal Google Sheet if DOWNLOAD_SHEET is True)
    if DOWNLOAD_SHEET:
        service = build_service()
        sheet = service.spreadsheets()
        try:
            result = sheet.values().get(spreadsheetId=settings.WINNMILL_CODEBOOKS_SPREADSHEET_ID, range='map_maternal!A1:K').execute()
        except HttpError as e:
            print(f'An error occurred trying to download the map_maternal sheet: {e}')
            pdb.set_trace()
        
        df = pd.DataFrame(result['values'][1:], columns=result['values'][0])
        df.to_csv(DOWNLOAD_PATH, index=False)
        print(f'map_maternal sheet downloaded and saved to {DOWNLOAD_PATH}')
    else:
        df = pd.read_csv(DOWNLOAD_PATH)
    
    # Create location map
    df_map = df.loc[df['indicator'] == 'delivery_location']
    
    # Initial Categorization Propositions
    # Level STEP 1: review based on name, using some of the common names below.
    # NOTE: order matters. Split gets evaluated first, then hospital, then primary care, etc.
    level_step1_kws = {
        # Split (e.g. "hospital/clinic")
        'split': ['hospital or mat', 'hop.*/mat.*', 'hosp-clin', 'hosp.*/.*clin', 'hospital/cent', 'hosp.*/.*dispensary', 'hosp.*/.*mat', 'hosp.*/.*clín'],
        # Hospital
        'hosp': ['special medical college', 'victoria jubilee', 'hf level [2|3]', 'aprofam', 'hospital', 'hopital', 'hôpital', 'surgical health center', 'cma \(centre medical ave antenne chirurgical\)', 'referral', 'hosp', 'maternity hospital', 'second level', 'royal medical services', 'health complex', 'diagnostic cent'],
        # Primary care
        'prim': ['ulaps', 'capps', 'shc', 'shp', 'puesto', 'posyandu', 'phong kham', 'phòng khám', 'hf level 1', 'welfare cent', 'general practitioner', 'fhu', 'fhc', 'family planning', 'ebais', 'cent.*de[\s|_]saúd', 'care cent', 'case de santé', 'centro.*de.*s.*l.*d', 'centro atencion', 'farm[a|i]c', 'bhu', 'basic', 'sub[\s|_|-]cent', 'nursing', 'doctor', 'med.*cent', 'cabinet', 'consult', 'centr.*medic', 'mou', 'pmi', 'cms', 'fap', 'vil', 'cent.*medicalis', 'delivery house', 'delivery home', 'peripheral unit', 'nursing.*matern', 'nursing.*home', 'nursing.*house', 'matern.*home', 'matern.*house', 'matern', 'centre de sant', 'dispens', 'clnic', 'cliniq', 'clínic', 'clinic', 'physician office', 'health post', 'health hut', 'health cabin', 'health cabinet', 'health house', 'health unit', 'outpatient', 'mobile outreach', 'clin', 'off', 'medical cent', 'health[\s|_]cent', 'h.*lth[\s|_]ctr' 'hlth[\s|_]cent', 'primary health care center', 'centre de sante de base', 'cabinet de soins', 'case de sante', 'post', 'chc', 'phc', 'phu', 'clinic/health center', 'first level', 'community health cetner', 'vil.*del.*p.*st', 'village unit', 'station', 'cs', 'puskesmas/polindes/pustu', 'sub center', 'pharmacy'],
        'home': ['parto dom', 'home', 'domicil', 'house', 'familiar', 'amigo'],
        'other': ['worker'] # Not actually included in logic
        # If no level or Unsure, move to step 2|3
    }
    # Generate regex from lists of keywords
    for lvl, kws_list in level_step1_kws.items():
        level_step1_kws[lvl] = '|'.join(kws_list)
    
    # Generate boolean flags for each level based on regex search of value column
    for lvl, regex in level_step1_kws.items():
        col_name = f'flag_level_{lvl}'
        df_map[col_name] = df_map['value'].str.contains(regex, case=False, na=False)
        
    # Ownership STEP 1: review based on name, using some of the common names below.
    # NOTE: order matters.
    ownership_step1_kws = {
        # NGO/Mission
        'ngo': ['ppag', 'umn', 'unrwa', 'beneficencia', 'egyptian fp assoc', 'egypt family planning assoc', 'charity', 'ashonplafa', 'blm', 'smiling sun', 'chal', 'csi', 'non-government', 'ngo', 'private non-profit', 'relig', 'mission', 'church', 'mosque', 'ong', 'foundation', 'fundacion', 'trust', 'faith', 'confes', 'voluntary', 'marie stopes', 'red cross', 'cruz roja'],
        # Public
        'public': ['imss', 'ssa', 'seguro.*social', 'isss', 'inamps', 'indhira gandhi mem.*hosp', 'ulaps', 'capps', 'ukbm', 'thana', 'upazil', 'special medical college', 'shp', 'shc', 'army', 'police', 'victoria jubilee', 'royal medical services', 'posyandu', 'dkkv', 'peripheral', 'pat.*san jose', 'ministry of h', 'PÚBLICO', 'militar', 'policia', 'h.*family welfare center', 'mohp', 'family planning association of nepal', 'empresa medicas previsionales', 'inss', 'ebais', 'communal', 'caja', 'cnss', 'secretaria de salud', 'pmi', 'cms', 'msp', 'bps', 'union', 'tirana', 'cesar', 'cesamo', 'hospital secretaria de salud', 'clínica materno infantil del ss', 'de la\s+s\.s', 'ihss', 'sisca', 'outreach', 'mobile clin', 'minsal', 'bhu', 'rhc', 'rural', 'mch', 'khum', 'ips', 'ipss', 'eps', 'soc.*secur', 'ssk', 'de la .*ss', 'del .*ss', 'parast', 'iess', 'ssc', 'sespas', 'ffaa', 'ff\.aa', 'p\.n', 'idss', 'gobierno', 'imss', 'community health unit', 'cscom', 'comm.*hlth', 'comm.*health', 'community clinic', 'municipal', 'national', 'chr', 'chc', 'arrondissement', 'centre de santé de référence', 'csref', 'reference health cent', 'public', 'pública', 'gov', 'government', 'gouvernement', 'gouvernment', 'gouvernemnt', 'govt', 'gouv', 'gouvernemental', 'publica', 'parastatal', 'state', 'district', 'regional', 'provinc', 'central', 'min\. of health', 'moh', 'imss', 'military'],
        # Private
        'private': ['mutualist', 'general practitioner', 'tu nhan', 'tư nhân', 'particular', 'aprofam', 'igss', 'lay', 'mmcwa', 'private', 'priv', 'privé', 'privat', 'privado', 'privat', 'private for profit', 'prive', 'privee', 'privado', 'privada', 'pvt', 'prvt'],
        # No sector, or unsure
        'nosector': ['no sector', 'home', 'domicilio']
    }
    # Generate regex from lists of keywords
    for own, kws_list in ownership_step1_kws.items():
        ownership_step1_kws[own] = '|'.join(kws_list)
    
    # Generate boolean flags for each sector based on regex search of value column
    for own, regex in ownership_step1_kws.items():
        col_name = f'flag_ownership_{own}'
        df_map[col_name] = df_map['value'].str.contains(regex, case=False, na=False)
    
    # Generate proposed category_indicator_name based on flags
    # Format: {ownership}_{level}, or just one of {ownership} or {level} if the other is unclear or simply "unknown" if both level and ownership are unclear
    df_map['proposed_level'] = df_map.apply(get_proposed_level, axis=1)
    df_map['proposed_ownership'] = df_map.apply(get_proposed_ownership, axis=1)
    df_map['proposed_cig'] = df_map.apply(get_proposed_cig, axis=1)
    # Drop flag cols
    df_map.drop(columns=[col for col in df_map.columns if 'flag_' in col], inplace=True)
    df_map.drop(columns=['proposed_level', 'proposed_ownership'], inplace=True)
    # Cleaning
    df_map.fillna('', inplace=True)
    df_map['rowid'] = pd.to_numeric(df_map['rowid'])
    df_map['map__maternal_id'] = pd.to_numeric(df_map['map__maternal_id'])
    df_map = df_map.sort_values(by='rowid').reset_index(drop=True)
    # Save a copy of the map
    df_map.to_csv(MAP_PATH, index=False)
    
    if UPDATE_PROPOSED_SHEET:
        # Update the map_maternal sheet
        service = build_service()
        sheet = service.spreadsheets()
        try:
            sheet.values().update(spreadsheetId=settings.WINNOWER_DB_MAP__MATERNAL_UPDATES_SPREADSHEET_ID, range='proposed_updates!A1',  body={'values': [df_map.columns.tolist()]}, valueInputOption='RAW').execute()
            sheet.values().update(spreadsheetId=settings.WINNOWER_DB_MAP__MATERNAL_UPDATES_SPREADSHEET_ID, range='proposed_updates!A2',  body={'values': df_map.values.tolist()}, valueInputOption='RAW').execute()
        except HttpError as e:
            print(f'An error occurred trying to update the map_maternal sheet: {e}')
            pdb.set_trace()
        print(f'map_maternal sheet updated with proposed level, ownership, and CIGs')
