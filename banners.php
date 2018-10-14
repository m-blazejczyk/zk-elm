<?php

// +-----------+---------------+-----+-----+------+----------------+
// |        Id |       int(11) |  NO | PRI | NULL | auto_increment |
// |  IsSilent |    tinyint(4) |  NO |     |    0 |                |
// | StartDate |          date | YES |     | NULL |                |
// |   EndDate |          date | YES |     | NULL |                |
// |       Url | varchar(1024) | YES |     | NULL |                |
// |    Weight |   smallint(6) |  NO |     |   10 |                |
// | ObrazekId |       int(11) | YES |     | NULL |                |
// +-----------+---------------+-----+-----+------+----------------+

/////////////////////////////////////////////////
// Get all banners
// Returns JSON
$router->map( 'GET', '/banners', withAuth0( function( $query ) {

    $query->setTable( "Bannery" );
    $result = $query->getStar();
    $response = array();
    while( $row = mysql_fetch_assoc( $result ) ) {
        $banner = array(
            'id' => intval( $row[ "Id" ] ),
            'isSilent' => ( $row[ "IsSilent" ] == "1" ),
            'startDate' => $row[ "StartDate" ],
            'endDate' => $row[ "EndDate" ],
            'imageUrl' => $row[ "ImageUrl" ],
            'imageHeight' => ( is_null( $row[ "ImageHeight" ] ) ? NULL : intval( $row[ "ImageHeight" ] ) ),
            'imageWidth' => ( is_null( $row[ "ImageWidth" ] ) ? NULL : intval( $row[ "ImageWidth" ] ) ),
            'url' => $row[ "Url" ],
            'weight' => intval( $row[ "Weight" ] ) );
        array_push( $response, $banner );
    }

    header( 'Content-Type: application/json' );
    echo json_encode( $response );

  } ) );

/////////////////////////////////////////////////
// Insert a new banner
// Returns JSON
$router->map( 'POST', '/banners', withAuth0( function( $query ) {

    $query->setTable( "Bannery" );
    // We need at least one field.  The rest will use defaults.
    $query->addInsert( "IsSilent", 0 );
    $query->insert();
    $id = intval( $query->lastInsertId() );
    $query->close();

    $banner = array(
      'id' => $id, 'isSilent' => false,
      'startDate' => null, 'endDate' => null,
      'imageUrl' => null, 'imageHeight' => null, 'imageWidth' => null,
      'url' => null, 'weight' => 10 );

    header( 'Content-Type: application/json' );
    echo json_encode( $banner );

  } ) );

/////////////////////////////////////////////////
// Delete a banner
// Returns HTTP code
$router->map( 'DELETE', '/banners/[i:id]', withAuth1Id( function( $query, $id ) {

    $query->setTable( "Bannery" );
    http_response_code( $query->deleteEx( "Id = " . $id ) ? 200 : 400 );
    $query->close();

  } ) );

/////////////////////////////////////////////////
// Edit a field in a banner
// 'PUT' does not work - $_REQUEST is not available
// Returns HTTP code
$router->map( 'POST', '/banners/[i:id]/edit', withAuth1Id( function( $query, $id ) {

    $query->setTable( "Bannery" );
    $query->addWhere( "Id = " . $id );

    if( isset( $_REQUEST[ 'silent' ] ) ) {
      $query->addInsert( 'IsSilent', $_REQUEST[ 'silent' ] == 'True' ? 1 : 0 );
    }
    if( isset( $_REQUEST[ 'startDate' ] ) ) {
      $value = $_REQUEST[ 'startDate' ];
      $query->addInsert( 'StartDate', $value == '' ? 'NULL' : $query->prepareStringBasic( $value ) );
    }
    if( isset( $_REQUEST[ 'endDate' ] ) ) {
      $value = $_REQUEST[ 'endDate' ];
      $query->addInsert( 'EndDate', $value == '' ? 'NULL' : $query->prepareStringBasic( $value ) );
    }
    if( isset( $_REQUEST[ 'url' ] ) ) {
      $value = $_REQUEST[ 'url' ];
      $query->addInsert( 'Url', $value == '' ? 'NULL' : $query->prepareStringBasic( $value ) );
    }
    if( isset( $_REQUEST[ 'weight' ] ) ) {
      $query->addInsert( 'Weight', intval( $_REQUEST[ 'weight' ] ) );
    }

    http_response_code( $query->updateEx() ? 200 : 400 );

    $query->close();

  } ) );

/////////////////////////////////////////////////
// Upload the image for a banner
// Returns JSON
$router->map( 'POST', '/banners/[i:id]/upload', withAuth1Id( function( $query, $id ) {

    if( isset( $_FILES[ 'SelectedFile' ] ) ) {
      // Check for errors
      if( $_FILES[ 'SelectedFile' ][ 'error' ] != UPLOAD_ERR_OK ) {
        jsonUploadError( 'Wystąpił problem podczas nagrywania pliku. Spróbuj jeszcze raz.' );
      } else if( $_FILES[ 'SelectedFile' ][ 'type' ] != 'image/png'
        && $_FILES[ 'SelectedFile' ][ 'type' ] != 'image/jpeg' ) {
        jsonUploadError( 'Niewłaściwy typ pliku.  Można nagrywać tylko pliki PNG i JPEG.' );
      } else if( !getimagesize( $_FILES[ 'SelectedFile' ][ 'tmp_name' ] ) ) {
        jsonUploadError( 'Niewłaściwy format pliku.  Może to nie jest obrazek?' );
      } else if( $_FILES[ 'SelectedFile' ][ 'size' ] > 10000000 ) {
        jsonUploadError( 'Plik jest za duży. Maksymalna wielkość pliku to 10 MB.' );
      } else {
        // Process the full image.
        $img = new SimpleImageOld();
        $img->load( $_FILES[ 'SelectedFile' ][ 'tmp_name' ] );
        $fullW = $img->getWidth();
        $fullH = $img->getHeight();

        if( $fullW == 200 ) {
          // Don't resize the file (and keep the original format) if it's exactly 200 pixels wide.
          $isFullResized = false;
          $resizedW = $fullW;
          $resizedH = $fullH;
          $resizedExt = ( $_FILES[ 'SelectedFile' ][ 'type' ] == 'image/png' ? '.png' : '.jpg' );
        } else {
          $isFullResized = true;
          $img->resizeToWidth( 200 );
          $resizedW = $img->getWidth();
          $resizedH = $img->getHeight();
          $resizedExt = '.jpg';
        }

        // Generate a random file name for the target.
        while( true ) {
          $random = randomUploadPath( 20 );
          $targetFull = 'static/upload/' . $random . $resizedExt;

          // Check if the file exists
          if( !file_exists( $targetFull ) ) {
            break;
          }
        }

        if( $isFullResized ) {
          $img->save( $targetFull, IMAGETYPE_JPEG, 98 );
        } else {
          move_uploaded_file( $_FILES[ 'SelectedFile' ][ 'tmp_name' ], $targetFull );
        }
        $img->destroy();

        // Success!
        jsonUploadSuccess( $targetFull, $resizedW, $resizedH );
      }
    } else {
      error_log( 'File upload: Missing form field SelectedFile.' );
      http_response_code( 500 );
    }
  } ) );

?>
